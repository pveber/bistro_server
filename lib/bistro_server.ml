open Core.Std
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Tyxml_html
open Bistro_server_common

let digest x =
  Marshal.to_string x []
  |> Digest.string
  |> Digest.to_hex

let head t =
  head (title (pcdata t)) [
    (* link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ; *)
    (* link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ; *)
    (* script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ; *)
    (* script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ; *)
    Unsafe.(node "script" ~a:[string_attrib "type" "text/javascript"] [ data Bistro_server_js.contents]) ;
  ]

let response title contents =
  html
    (head title)
    (body [])

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
    | In_progress
    | Completed
    | Errored

  type run = {
    id : string ;
    input : App.input ;
    state : run_state ;
    repo : Bistro_repo.t ;
  }

  let app_specification = {
    app_title = App.title ;
    app_form = App.form ;
  }

  let current_runs = String.Table.create ()

  let update_run_state id s =
    String.Table.update current_runs id ~f:(function
        | Some r -> { r with state = s }
        | None -> assert false
      )

  let build_process run =
    let outdir = Filename.concat "res" run.id in
    let term = Bistro_repo.to_app ~outdir run.repo in
    Bistro_app.create term >|= function
    | Ok () -> update_run_state run.id Completed
    | Error _ -> update_run_state run.id Errored

  let new_run input =
    let id = digest input in
    let r = { id ; input ; state = In_progress ; repo = App.derive input } in
    String.Table.set current_runs ~key:id ~data:r ;
    id

  let handler meth path body =
    match meth, path with
    | `GET, [""] ->
      return (
        `OK,
        response "Bistro Web Server" []
        |> render
      )

    | `GET, ["app_specification"] ->
      let body =
        app_specification
        |> sexp_of_app_specification
        |> Sexp.to_string_hum
      in
      return (`OK, body)

    | `POST, ["run"] ->
      Cohttp_lwt_body.to_string body >|= fun body ->
      (
        try
          let input = App.input_of_sexp (Sexp.of_string body) in
          let id = new_run input in
          `OK, id
        with Failure s ->
          `Bad_request, s
      )

    | _ ->
      return (`Not_found, "Not found")

  let server () =
    let callback _conn req body =
      let uri = Request.uri req in
      let path = uri |> Uri.path |> String.split ~on:'/' |> List.tl_exn in
      let meth = Request.meth req in
      handler meth path body >>= fun (status, body) ->
      Server.respond_string ~status ~body ()
    in
    Server.make ~callback ()

  let start () =
    Server.create ~mode:(`TCP (`Port 8000)) (server ())
end
