open Js_browser

let start _ =
  Window.alert window "Started"

let _ =
  Window.set_onload window start
