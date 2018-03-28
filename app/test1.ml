open Core
open Bistro.EDSL
open Bistro_utils

module X = struct
  type input = {
    a : int ;
    b : string ;
    c : c ;
  }
  and c = {
    d : string ;
    e : string [@file] ;
  }
  [@@deriving sexp, bistro_form]

  let title = "App title"

  let echo x =
    workflow ~descr:"echo" [
      cmd "echo" ~stdout:dest [ string x ]
    ]

  let derive ~data:_ _ = Repo.[
      [ "a" ] %> echo "a" ;
      [ "b" ] %> echo "b" ;
      [ "c" ; "d" ] %> echo "d" ;
    ]

end

module Server = Bistro_server.Make(X)

let () = Lwt_main.run (Server.start ())
