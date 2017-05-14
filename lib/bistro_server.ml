open Lwt
open Cohttp
open Cohttp_lwt_unix
open Tyxml_html

let head t =
  head (title (pcdata t)) [
    link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap.min.css" () ;
    link ~rel:[`Stylesheet] ~href:"http://netdna.bootstrapcdn.com/bootstrap/3.0.2/css/bootstrap-theme.min.css" () ;
    script ~a:[a_src "https://code.jquery.com/jquery.js"] (pcdata "") ;
    script ~a:[a_src "http://netdna.bootstrapcdn.com/bootstrap/3.0.2/js/bootstrap.min.js"] (pcdata "") ;
    Unsafe.(node "script" ~a:[string_attrib "type" "text/javascript"] [ data Bistro_server_js.contents]) ;
  ]

let response title contents =
  html
    (head title)
    (body [ div ~a:[a_class ["container"]] contents ])

let render doc =
  let buf = Buffer.create 253 in
  let formatter = Format.formatter_of_buffer buf in
  Tyxml_html.pp () formatter doc ;
  Buffer.contents buf


module Make(X : sig end) = struct

  let server () =
    let callback _conn req body =
      let uri = req |> Request.uri |> Uri.to_string in
      let meth = req |> Request.meth |> Code.string_of_method in
      let headers = req |> Request.headers |> Header.to_string in
      body |> Cohttp_lwt_body.to_string >|= (fun body ->
          (Printf.sprintf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\nBody: %s"
             uri meth headers body))
      >>= (fun body ->
          let response = response "title" [ pcdata body ] in
          Server.respond_string ~status:`OK ~body:(render response) ())
    in
    Server.make ~callback ()

  let start () =
    Server.create ~mode:(`TCP (`Port 8000)) (server ())
end
