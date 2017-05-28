open Core
open Bistro.EDSL

module X = struct
  type input = {
    a : int ;
    b : string ;
    c : c ;
  }
  and c = {
    d : string ;
    e : string ;
  }
  [@@deriving sexp, bistro_form]

  let title = "App title"

  (* let form = Bistro_server_common.{ *)
  (*   fields = [ *)
  (*     ("a", Int_field (Some 42)) ; *)
  (*     ("b", String_field None) ; *)
  (*     ("c", Form_field { *)
  (*         fields = [ *)
  (*           ("d", String_field (Some "foobar")) ; *)
  (*           ("e", File_field None) ; *)
  (*         ] *)
  (*       }) ; *)
  (*   ] *)
  (* } *)

  let echo x =
    workflow ~descr:"echo" [
      cmd "echo" ~stdout:dest [ string x ]
    ]

  let derive x = Bistro_repo.[
      [ "a" ] %> echo "a" ;
      [ "b" ] %> echo "b" ;
      [ "c" ; "d" ] %> echo "d" ;
    ]

end

module Server = Bistro_server.Make(X)

let () = Lwt_main.run (Server.start ())
