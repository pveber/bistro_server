open Sexplib.Std
open Js_browser
open Bistro_server_common

let ( >>= ) = Lwt.( >>= )
let ( >>| ) = Lwt.( >|= )

let http_request path =
  let waiter, wakener = Lwt.wait () in
  let uri = String.concat "" [
      "http://" ;
      window |> Window.location |> Location.host ;
      "/" ;
      String.concat "/" path ;
    ]
  in
  let xhr = XHR.create () in
  XHR.set_onreadystatechange xhr (fun () ->
      match XHR.ready_state xhr with
      | XHR.Done -> Lwt.wakeup wakener (XHR.response_text xhr)
      | _ -> ()
    ) ;
  XHR.open_ xhr "GET" uri ;
  XHR.send xhr "" ;
  waiter

let form_view { fields } =
  ()

let update m () =
  m, Vdom.Cmd.Batch []

let view spec =
  let open Vdom in
  text spec.app_title

let main spec =
  let init = spec, Vdom.Cmd.Batch [] in
  let app = Vdom.app ~init ~update ~view () in
  Vdom_blit.run app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let onload _ =
  Lwt.async (fun () ->
      http_request ["app_specification"]
      >>| Sexplib.Sexp.of_string
      >>| app_specification_of_sexp
      >>| main
    )

let _ =
  Window.set_onload window onload
