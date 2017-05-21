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

let head ~js t =
  let app_js =
    Unsafe.(
      node "script"
        ~a:[string_attrib "type" "text/javascript"]
        [ data Bistro_server_js.contents]
    )
  in
  let contents = List.concat [
      if js then [ app_js ] else []
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
    | In_progress
    | Completed
    | Errored
  [@@deriving sexp]

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
    Lwt.async (fun () -> build_process r) ;
    id


  let run_list_summary runs =
    let table =
      table @@ List.map (String.Table.data runs) ~f:(fun r ->
          tr [ td [a ~a:[a_href ("run/" ^ r.id)] [pcdata r.id]] ]
        )
    in
    html_page ~js:false "Bistro Web Server: list of current runs" [
      h2 [ pcdata "Current runs" ] ;
      table ;
    ]

  let handler meth path body =
    match meth, path with
    | `GET, [""] ->
      return (
        `OK,
        html_page "Bistro Web Server" []
        |> render
      )

    | `GET, ["app_specification"] ->
      let body =
        app_specification
        |> sexp_of_app_specification
        |> Sexp.to_string_hum
      in
      return (`OK, body)

    | `GET, "runs" :: ([] | "/" :: []) ->
      let body = render (run_list_summary current_runs) in
      return (`OK, body)

    | `GET, "run" :: run_id :: _ -> (
        match String.Table.find current_runs run_id with
        | None -> return (`Not_found, "Unknown run")
        | Some run ->
          let body =
            run.state
            |> sexp_of_run_state
            |> Sexplib.Sexp.to_string
          in
      return (`OK, body)
      )

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
