open Core.Std
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

let list_remove xs x =
  List.filter xs ~f:(( = ) x)

let string_of_mime_type = function
  | `Text_plain -> "text/plain"
  | `Text_html -> "text/html"

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
  [@@deriving sexp]

  val title : string
  val form : form
  val derive : input -> Bistro_repo.t
end

module Make(App : App) = struct

  type run_state =
    | Init
    | Data_upload of {
        needed : string list ;
        started : string list ;
        uploaded : string list ;
        completed : unit Lwt.u ;
        wait_for_completion : unit Lwt.t ;
      }
    | Repo_build of {
        wait_for_completion : (unit, string) result Lwt.t ;
      }
    | Completed
    | Errored

  type showable_run_state = [
    | `Init
    | `Data_upload
    | `Repo_build
    | `Completed
    | `Errored
  ]
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
    app_form = App.form ;
  }

  let showable_run_state = function
    | Init -> `Init
    | Data_upload _ -> `Data_upload
    | Repo_build _ -> `Repo_build
    | Completed -> `Completed
    | Errored -> `Errored

  module Run_table :
  sig
    val create_entry : App.input run_request -> string
    val get : string -> run option
    val get_all : unit -> run list
    val signal :
      string ->
      [ `Start
      | `Started_upload of string (* file id *)
      | `Completed_upload of string ] -> (* file id *)
      (unit, [> `No_such_run_id]) Pervasives.result
  end
  =
  struct
    let runs = String.Table.create ()

    let get id = String.Table.find runs id

    let get_all () = String.Table.data runs

    let update_run_state id s =
      String.Table.update runs id ~f:(function
          | Some r -> { r with state = s }
          | None -> assert false
        )

    let start_build_process run =
      let outdir = Filename.concat "res" run.id in
      let term = Bistro_repo.to_app ~outdir run.repo in
      let wait_for_completion = Bistro_app.create term in
      Repo_build { wait_for_completion }

    let update run evt =
      match run.state, evt with
      | Init, `Start -> (
          match run.input_files with
          | [] -> start_build_process run
          | _ :: _ ->
            let wait_for_completion, completed = Lwt.wait () in
            Data_upload {
              needed = List.map run.input_files ~f:(fun fn -> fn.input_file_id) ;
              started = [] ;
              uploaded = [] ;
              wait_for_completion ;
              completed ;
            }
        )
      | Data_upload up, `Started_upload file_id ->
        assert (List.mem up.needed file_id) ;
        Data_upload {
          up with needed = list_remove up.needed file_id ;
                  started = file_id :: up.started ;
        }
      | Data_upload up, `Completed_upload file_id ->
        assert (List.mem up.started file_id) ;
        let started = list_remove up.started file_id in
        if started = [] && up.needed = [] then
          start_build_process run
        else
          Data_upload {
            up with started ;
                    uploaded = file_id :: up.uploaded ;
          }
      | _ -> assert false

    let signal id evt =
      match String.Table.find runs id with
      | None -> Pervasives.Error `No_such_run_id
      | Some run ->
        let new_state = update run evt in
        update_run_state id new_state ;
        Ok ()

    let create_entry { input ; files } =
      let id = digest input in
      let r = {
        id ; input ; input_files = files ;
        state = Init ;
        repo = App.derive input
      }
      in
      String.Table.set runs ~key:id ~data:r ;
      ignore (signal id `Start) ;
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
      return_html @@ run_list_summary @@ Run_table.get_all ()

    | `GET, "run" :: run_id :: _ -> (
        match Run_table.get run_id with
        | None -> return_not_found "Unknown run"
        | Some run ->
          run.state
          |> showable_run_state
          |> sexp_of_showable_run_state
          |> Sexplib.Sexp.to_string
          |> return_text
      )

    | `POST, ["run"] ->
      Cohttp_lwt_body.to_string body >>= fun body ->
      (
        try
          let req = run_request_of_sexp App.input_of_sexp (Sexp.of_string body) in
          let id = Run_table.create_entry req in
          return_text id
        with Failure s ->
          return (`Bad_request, s, `Text_plain)
      )

    | `POST, ["upload" ; run_id ; file_id ] -> (
        match Run_table.get run_id with
        | None -> return (`Bad_request, "Unknown run id", `Text_plain)
        | Some { state = Data_upload up } ->
          if List.mem up.needed file_id then (
            let open Lwt_io in
            ignore @@ Run_table.signal run_id (`Started_upload file_id) ;
            let dir = Filename.concat "data" run_id in
            let file = Filename.concat dir file_id in
            Lwt_unix.mkdir dir 0o755 >>= fun () ->
            with_file ~mode:output file @@ fun oc ->
            Cohttp_lwt_body.write_body (write oc) body >>= fun () ->
            ignore @@ Run_table.signal run_id (`Completed_upload file_id) ;
            return_text ""
          )
          else
            let msg = sprintf "%s is not needed for %s" file_id run_id in
            return (`Bad_request, msg, `Text_plain)
        | Some { state = (Init | Repo_build _ | Completed | Errored) } ->
          let msg = sprintf "Not expecting any upload for run %s" run_id in
          return (`Bad_request, msg, `Text_plain)
      )

    | _ ->
      return_not_found "Not found"

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
