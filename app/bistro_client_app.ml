module Html = Dom_html

let js = Js.string

let start _ =
  Html.window##alert (js"GAME OVER") ;
  Js._false

let _ =
  Html.window##.onload := Html.handler start
