open Cohttp_lwt_unix
open Bistro_server_common

module type App = sig
  type input
  [@@deriving sexp, bistro_form]

  val title : string
  val form : form
  val derive : input -> Bistro_repo.t
end

module Make(App : App) : sig
  val start : unit -> unit Lwt.t
end
