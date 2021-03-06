(* FIXME:
   should create data directory at startup
*)
open Core
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Tyxml_html
open Bistro_server_common
open Bistro_utils

type ('a, 'b) result = ('a, 'b) Pervasives.result = Ok of 'a | Error of 'b

type server_config = {
  np : int option ;
  mem : [`GB of int] option ;
  build_log : bool ;
  port : int ;
  root_dir : string ;
}


(* This [daemonize] function was taken from

   https://github.com/xapi-project/xen-api-libs/

   under the LGPL license.
*)
let daemonize () =
  let open Caml in
  match Unix.fork () with
  | 0 ->
    if Unix.setsid () == -1 then
      failwith "Unix.setsid failed";

    begin match Unix.fork () with
      | 0 ->
        let nullfd = Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0 in
        begin try
            Unix.close Unix.stdin;
            Unix.dup2 nullfd Unix.stdout;
            Unix.dup2 nullfd Unix.stderr;
          with exn -> Unix.close nullfd; raise exn
        end;
        Unix.close nullfd
      | _ -> exit 0
    end
  | _ -> exit 0


let digest x =
  Marshal.to_string x []
  |> Md5.digest_string
  |> Md5.to_hex

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
      [
        link ~rel:[`Stylesheet] ~href:"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css" () ;
        script ~a:[a_src "https://code.jquery.com/jquery-3.2.1.slim.min.js"] (txt "") ;
        script ~a:[a_src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.3/umd/popper.min.js"] (txt "") ;
        script ~a:[a_src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/js/bootstrap.min.js"] (txt "") ;
      ] ;
    ]
  in
  head (title (txt t)) contents


let html_page ?(js = true) title contents =
  html
    (head ~js title)
    (body [div ~a:[a_class ["container"]] contents])

let render doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Buffer.contents buf


type response = {
  status : Code.status_code ;
  headers : Header.t ;
  body : string ;
}

let response ?(headers = []) ?mime_type status body =
  let headers =
    List.concat [
      Option.value_map mime_type ~default:[] ~f:(fun ty ->
          [ "Content-Type", string_of_mime_type ty ]
        ) ;
      headers ;
    ]
    |> Header.of_list
  in
  { status ; headers ; body }

let return_html p =
  return (response `OK ~mime_type:`Text_html (render p))

let return_text t =
  return (response `OK ~mime_type:`Text_plain t)

let return_not_found msg =
  return (response `Not_found ~mime_type:`Text_plain msg)

let return_file ?mime_type path =
  if Sys.file_exists path = `Yes then
    let contents = In_channel.read_all path in (* FIXME: sync read *)
    return (response ?mime_type `OK contents)
  else
    return_not_found (sprintf "File %s does not exist" path)

let return_sexp conv v =
  let contents =
    conv v
    |> Sexplib.Sexp.to_string
  in
  return (response `OK ~mime_type:`Text_plain contents)

let dir config path =
  List.reduce_exn ~f:Filename.concat (config.root_dir :: path)

let upload_dir config ~run_id =
  dir config ["runs" ; run_id ; "uploads"]

let result_dir config ~run_id =
  dir config ["runs" ; run_id ; "res"]

let bistro_dir config ~run_id =
  dir config ["runs" ; run_id ; "_bistro"]

module type App = sig
  type input
  [@@deriving sexp, bistro_form]

  val title : string
  val input_bistro_form : form
  val derive :
    data:(string -> string) ->
    input ->
    Repo.t
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
    build_status : Build_status.t option ;
    repo : Repo.t ;
  }

  let app_specification = {
    app_title = App.title ;
    app_form = App.input_bistro_form ;
  }

  module State :
  sig
    val start_run :
      server_config ->
      App.input run_request ->
      string
    val get_run : string -> run option
    (* val get_run_exn : string -> run *)
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
    (* let get_run_exn id = String.Table.find_exn runs id *)

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

    let logger _id = object
      method event _ _ ev =
        (* let store e =
         *   let run = get_run_exn id in
         *   let build_status = Option.map run.build_status ~f:(fun status ->
         *       {
         *         status with
         *         log = e :: List.filter status.log ~f:Build_log_entry.(fun x ->
         *             x.id <> e.id
         *           ) ;
         *         nb_completed_steps =
         *           status.nb_completed_steps + if e.status = `DONE then 1 else 0 ;
         *         nb_failed_steps =
         *           status.nb_failed_steps + if e.status = `FAILED then 1 else 0 ;
         *       }
         *     )
         *   in
         *   let run = { run with build_status } in
         *   String.Table.set runs ~key:id ~data:run
         * in *)
        match ev with
        | _ -> ()
        (* | Bistro_engine.Logger.Init { needed ; already_done } ->
         *   let build_status = Some Build_status.{
         *     log = [] ;
         *     nb_steps = List.length needed ;
         *     nb_completed_steps = List.length already_done ;
         *     nb_failed_steps = 0 ;
         *   }
         *   in
         *   update_run_state id Repo_build ;
         *   let run = get_run_exn id in
         *   String.Table.set runs ~key:id ~data:{ run with build_status } *)

        (* | Bistro_engine.Logger.Workflow_ready _
         * | Workflow_started (Input _, _) -> ()
         * | Workflow_ended (Input_check _ | Select_check _)
         * | Workflow_skipped _ -> ()
         * | Workflow_started (Bistro.Step { id ; descr }, _) ->
         *   let e = Build_log_entry.{
         *       id ;
         *       descr ;
         *       status = `STARTED ;
         *     }
         *   in
         *   store e
         * | Workflow_ended (Step_result { outcome ; step }) ->
         *   let e = Build_log_entry.{
         *       id = step.id ;
         *       descr = step.descr ;
         *       status = (
         *         match outcome with
         *         | `Succeeded -> `DONE
         *         | `Failed | `Missing_output -> `FAILED
         *       ) ;
         *     }
         *   in
         *   store e
         * | Workflow_ended (Map_command_result { pass ; step }) ->
         *   let e = Build_log_entry.{
         *       id = step.id ;
         *       descr = step.descr ;
         *       status = (
         *         if pass then `DONE else `FAILED
         *       ) ;
         *     }
         *   in
         *   store e *)
      method stop = Lwt.return () (* FIXME: should stop modifying run *)
    end

    let start_run config { input ; files } =
      let run_id = digest input in
      let data fn = Filename.concat (upload_dir config ~run_id) fn in
      let r = {
        id = run_id ; input ; input_files = files ;
        state = Init ;
        build_status = None ;
        repo = App.derive ~data input
      }
      in
      String.Table.set runs ~key:run_id ~data:r ;
      Lwt.async (fun () ->
          update_run_state run_id Data_upload ;
          List.map files ~f:(fun fn ->
              let wait_for_upload, uploaded = Lwt.wait () in
              U.set uploads
                ~key:(run_id, fn.input_file_id)
                ~data:(`Needed uploaded) ;
              wait_for_upload
            )
          |> Lwt.join >>= fun () ->
          let outdir = result_dir config ~run_id:r.id in
          (* the logger sets the state to Repo_build *)
          let loggers = [ logger run_id ; Bistro_utils.Console_logger.create ()] in
          let thread =
            Bistro_utils.Repo.build
              ~outdir
              ?np:config.np
              ?mem:config.mem
              ~bistro_dir:(bistro_dir config ~run_id)
              ~loggers
              r.repo
          in
          Lwt.catch
            (fun () -> thread >|= fun () -> update_run_state run_id Completed)
            (fun _ -> update_run_state run_id (Errored "Failure") ; Lwt.return ())
        ) ;
      run_id

  end



  let run_list_summary runs =
    let table =
      table @@ List.map runs ~f:(fun r ->
          tr [ td [a ~a:[a_href ("run/" ^ r.id)] [txt r.id]] ]
        )
    in
    html_page ~js:false "Bistro Web Server: list of current runs" [
      h2 [ txt "Current runs" ] ;
      table ;
    ]

  let file_browser_page config run rel_path =
    let title = sprintf "Bistro Web Server: run %s" run.id in
    let html_page info = html_page ~js:false title info in
    let path =
      Filename.concat
        (result_dir config ~run_id:run.id)
        (string_of_path rel_path) in
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
          >|= List.sort ~compare:String.compare
          >|= List.map ~f:(fun fn ->
              let path = string_of_path @@ "/run" :: run.id :: rel_path @ [ fn ] in
              tr [ td [ a ~a:[a_href path] [ txt fn ] ] ]
            )
          >|= table
        in
        table
        >|= (fun x -> [ x ])
        >|= html_page
        >>= return_html
      | Unix.S_REG -> (
          let mime_type, _download =
            match snd (Filename.split_extension path) with
            | Some "html" -> Some `Text_html, false
            | _ -> None, true
          in
          return_file ?mime_type path
        )
      | _ -> assert false

  let run_state_page config run rel_path =
    match run.state with
    | Completed -> file_browser_page config run rel_path
    | Init
    | Data_upload
    | Repo_build ->
      let title = sprintf "Bistro Web Server: run %s" run.id in
      let html_page info = html_page ~js:false title info in
      run.state
      |> sexp_of_run_state
      |> Sexplib.Sexp.to_string_hum
      |> (fun x -> [ txt x ])
      |> html_page
      |> return_html
    | Errored msg ->
      return_text msg

  let handler config meth path body =
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

    | `GET, "run" :: "log" :: run_id :: [] -> (
        match State.get_run run_id with
        | None -> return_not_found "Unknown run"
        | Some { build_status = Some s ; _ } ->
          return_sexp Build_status.sexp_of_t s
        | Some { build_status = None ; _ } ->
          return_not_found "no build status" (* FIXME *)
      )

    | `GET, "run" :: run_id :: path -> (
        match State.get_run run_id with
        | None -> return_not_found "Unknown run"
        | Some run -> run_state_page config run path
      )

    | `POST, ["run"] ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      (
        try
          let req = run_request_of_sexp App.input_of_sexp (Sexp.of_string body) in
          let id = State.start_run config req in
          return_text id
        with Failure s ->
          return (response `Bad_request ~mime_type:`Text_plain s)
      )

    | `POST, ["upload" ; run_id ; file_id ] -> (
        match State.accept_download ~run_id ~file_id with
        | Ok notify_completion ->
          Lwt.catch (fun () ->
              let open Lwt_io in
              let upload_dir = upload_dir config ~run_id in
              Unix.mkdir_p upload_dir ; (* FIXME *)
              (* Lwt_unix.mkdir upload_dir 0o755 >>= fun () -> *)
              let file = Filename.concat upload_dir file_id in
              with_file ~mode:output file @@ fun oc ->
              Cohttp_lwt.Body.write_body (write oc) body >>= fun () ->
              notify_completion () ;
              return_text ""
            )
            (fun exn ->
               let msg = Exn.to_string exn in
               print_endline msg ;
               return (response `Internal_server_error ~mime_type:`Text_plain msg)
            )
        | Error msg ->
          return (response `Bad_request ~mime_type:`Text_plain msg)
      )

    | _ ->
      return_not_found "Page not found"

  let server config () =
    Unix.mkdir_p config.root_dir ;
    let callback _conn req body =
      let uri = Request.uri req in
      let path = uri |> Uri.path |> String.split ~on:'/' |> List.tl_exn in
      let meth = Request.meth req in
      handler config meth path body >>= fun { status ; body ; headers } ->
      Server.respond_string ~status ~headers ~body ()
    in
    Server.make ~callback ()

  let start ?(port = 8080) ?(build_log = false) ?(root_dir = "_bistro") ?np ?mem ?(daemon = false) () =
    let config = {
      port ; build_log ; np ; mem ; root_dir ;
    }
    in
    if daemon then daemonize () ;
    Server.create ~mode:(`TCP (`Port port)) (server config ())
end
