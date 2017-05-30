(* FIXME:
   should create data directory at startup
*)
open Core
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Tyxml_html
open Bistro_server_common

type ('a, 'b) result = ('a, 'b) Pervasives.result = Ok of 'a | Error of 'b

let digest x =
  Marshal.to_string x []
  |> Digest.string
  |> Digest.to_hex

let string_of_mime_type = function
  | `Text_plain -> "text/plain; charset=utf-8"
  | `Text_html -> "text/html"

let string_of_path = function
  | [] -> ""
  | _ :: _ as xs -> List.reduce_exn xs ~f:Filename.concat

let head ~js t =
  let app_js =
    Unsafe.(
      node "script"
        ~a:[string_attrib "type" "text/javascript"]
        [ data Bistro_server_js.contents]
    )
  in
  let contents = List.concat [
      [ meta ~a:[a_charset "utf-8"] () ] ;
      if js then [ app_js ] else [] ;
    ]
  in
  head (title (pcdata t)) contents

    (* link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ; *)
    (* link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ; *)
    (* script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ; *)
    (* script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ; *)

let html_page ?(js = true) title contents =
  html
    (head ~js title)
    (body contents)

let render doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Buffer.contents buf


module type App = sig
  type input
  [@@deriving sexp, bistro_form]

  val title : string
  val input_bistro_form : form
  val derive :
    data:(string -> string) ->
    input ->
    Bistro_repo.t
end

module Make(App : App) = struct

  type run_state =
    | Init
    | Data_upload
    | Repo_build
    | Completed
    | Errored of string
  [@@deriving sexp]

  type run = {
    id : string ;
    input : App.input ;
    input_files : input_file_descr list ;
    state : run_state ;
    repo : Bistro_repo.t ;
  }

  let app_specification = {
    app_title = App.title ;
    app_form = App.input_bistro_form ;
  }

  module State :
  sig
    val start_run : App.input run_request -> string
    val get_run : string -> run option
    val get_runs : unit -> run list

    val accept_download :
      run_id:string ->
      file_id:string ->
      (unit -> unit, string) result
  end
  =
  struct
    let runs = String.Table.create ()

    module U = Hashtbl.Make(struct
        include Tuple.Make(String)(String)
        include Tuple.Hashable(String)(String)
      end)

    let uploads = U.create ()

    let get_run id = String.Table.find runs id

    let get_runs () = String.Table.data runs

    let update_run_state id s =
      String.Table.update runs id ~f:(function
          | Some r -> { r with state = s }
          | None -> assert false
        )

    let accept_download ~run_id ~file_id =
      let key = run_id, file_id in
      match U.find uploads key with
      | None -> Error "Unknown upload"
      | Some (`Needed u) ->
        U.set uploads ~key ~data:(`Started u) ;
        Ok (
          fun () ->
            U.set uploads ~key ~data:`Completed ;
            Lwt.wakeup u ()
        )
      | Some (`Started _) -> Error "Already started"
      | Some `Completed -> Error "Already uploaded"

    let start_run { input ; files } =
      let id = digest input in
      let data fn = string_of_path [ "data" ; id ; fn ] in
      let r = {
        id ; input ; input_files = files ;
        state = Init ;
        repo = App.derive ~data input
      }
      in
      String.Table.set runs ~key:id ~data:r ;
      Lwt.async (fun () ->
          update_run_state id Data_upload ;
          List.map files ~f:(fun fn ->
              let wait_for_upload, uploaded = Lwt.wait () in
              U.set uploads
                ~key:(id, fn.input_file_id)
                ~data:(`Needed uploaded) ;
              wait_for_upload
            )
          |> Lwt.join >>= fun () ->
          update_run_state id Repo_build ;
          let outdir = string_of_path [ "res" ; r.id ] in
          let term = Bistro_repo.to_app ~outdir r.repo in
          Bistro_app.create term >|= function
          | Ok () -> update_run_state id Completed
          | Error msg -> update_run_state id (Errored msg)
        ) ;
      id
  end



  let run_list_summary runs =
    let table =
      table @@ List.map runs ~f:(fun r ->
          tr [ td [a ~a:[a_href ("run/" ^ r.id)] [pcdata r.id]] ]
        )
    in
    html_page ~js:false "Bistro Web Server: list of current runs" [
      h2 [ pcdata "Current runs" ] ;
      table ;
    ]

  let return_html p = return (`OK, render p, `Text_html)

  let return_text t = return (`OK, t, `Text_plain)

  let return_not_found msg =
    return (`Not_found, msg, `Text_plain)

  let run_state_page run rel_path =
    let title = sprintf "Bistro Web Server: run %s" run.id in
    let html_page info = html_page ~js:false title info in
    match run.state with
    | Completed -> (
        let path = string_of_path ("res" :: run.id :: rel_path) in
        Lwt_unix.file_exists path >>= function
        | false -> return_not_found "Path not found"
        | true ->
          Lwt_unix.stat path >>= fun stats ->
          match stats.Lwt_unix.st_kind with
          | Unix.S_DIR ->
            let table =
              path
              |> Lwt_unix.files_of_directory
              |> Lwt_stream.to_list
              >|= List.filter ~f:(function
                  | "."
                  | "_files" -> false
                  | ".." -> rel_path <> []
                  | _ -> true
                )
              >|= List.sort ~cmp:String.compare
              >|= List.map ~f:(fun fn ->
                  let path = string_of_path @@ "/run" :: run.id :: rel_path @ [ fn ] in
                  tr [ td [ a ~a:[a_href path] [ pcdata fn ] ] ]
                )
              >|= table
            in
            table
            >|= (fun x -> [ x ])
            >|= html_page
            >>= return_html
          | Unix.S_REG ->
            return_text (In_channel.read_all path)
          | _ -> assert false
      )
    | Init
    | Data_upload
    | Repo_build ->
      run.state
      |> sexp_of_run_state
      |> Sexplib.Sexp.to_string_hum
      |> (fun x -> [ pcdata x ])
      |> html_page
      |> return_html
    | Errored msg ->
      return_text msg

  let handler meth path body =
    match meth, path with
    | `GET, [""] ->
      return_html @@ html_page "Bistro Web Server" []

    | `GET, ["app_specification"] ->
      app_specification
      |> sexp_of_app_specification
      |> Sexp.to_string_hum
      |> return_text

    | `GET, "runs" :: ([] | "/" :: []) ->
      return_html @@ run_list_summary @@ State.get_runs ()

    | `GET, "run" :: run_id :: path -> (
        match State.get_run run_id with
        | None -> return_not_found "Unknown run"
        | Some run -> run_state_page run path
      )

    | `POST, ["run"] ->
      Cohttp_lwt_body.to_string body >>= fun body ->
      (
        try
          let req = run_request_of_sexp App.input_of_sexp (Sexp.of_string body) in
          let id = State.start_run req in
          return_text id
        with Failure s ->
          return (`Bad_request, s, `Text_plain)
      )

    | `POST, ["upload" ; run_id ; file_id ] -> (
        match State.accept_download ~run_id ~file_id with
        | Ok notify_completion ->
          Lwt.catch (fun () ->
              let open Lwt_io in
              let dir = Filename.concat "data" run_id in
              let file = Filename.concat dir file_id in
              Lwt_unix.mkdir dir 0o755 >>= fun () ->
              with_file ~mode:output file @@ fun oc ->
              Cohttp_lwt_body.write_body (write oc) body >>= fun () ->
              notify_completion () ;
              return_text ""
            )
            (fun exn ->
               let msg = Exn.to_string exn in
               return (`Internal_server_error, msg, `Text_plain)
            )
        | Error msg ->
          return (`Bad_request, msg, `Text_plain)
      )

    | _ ->
      return_not_found "Page not found"

  let server () =
    let callback _conn req body =
      let uri = Request.uri req in
      let path = uri |> Uri.path |> String.split ~on:'/' |> List.tl_exn in
      let meth = Request.meth req in
      handler meth path body >>= fun (status, body, mime) ->
      let headers = Header.of_list ["Content-Type", string_of_mime_type mime] in
      Server.respond_string ~status ~headers ~body ()
    in
    Server.make ~callback ()

  let start () =
    Server.create ~mode:(`TCP (`Port 8000)) (server ())
end
