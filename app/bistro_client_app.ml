open Js_browser

let start _ =
  let host =
    Window.location window
    |> Location.host
  in
  Window.alert window host

let _ =
  Window.set_onload window start
