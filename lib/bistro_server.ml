open Core.Std
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Tyxml_html
open Bistro_server_common

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


module Make(X : sig end) = struct

  let app_specification = {
    app_title = "App title" ;
    app_form = {
      fields = [
        ("a", Int_field (Some 42)) ;
        ("b", String_field None) ;
        ("c", Form_field {
            fields = [
              ("d", String_field (Some "foobar")) ;
            ]
          }) ;
      ]
    }
  }

  let handler meth path =
    match meth, path with
    | `GET, [""] ->
      `OK,
      response "Bistro Web Server" []
      |> render
    | `GET, ["app_specification"] ->
      `OK, Sexp.to_string_hum (sexp_of_app_specification app_specification)
    | _ ->
      `Not_found, "Not found"

  let server () =
    let callback _conn req body =
      let uri = Request.uri req in
      let path = uri |> Uri.path |> String.split ~on:'/' |> List.tl_exn in
      let meth = Request.meth req in
      let status, body = handler meth path in
      Server.respond_string ~status ~body ()
    in
    Server.make ~callback ()

  let start () =
    Server.create ~mode:(`TCP (`Port 8000)) (server ())
end
