open Bistro_server_common

module type App = sig
  type input
  [@@deriving sexp, bistro_form]

  val title : string
  val input_bistro_form : form
  val derive :
    data:(string -> string) ->
    input ->
    Bistro_repo.t
end

module Make(App : App) : sig
  val start : unit -> unit Lwt.t
end
