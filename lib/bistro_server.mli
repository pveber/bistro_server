open Cohttp_lwt_unix
open Bistro_server_common

module type App = sig
  type input
  [@@deriving sexp]

  val title : string
  val form : form
end

module Make(App : App) : sig
  val start : unit -> unit Lwt.t
end
