open Sexplib.Std
open Bistro.EDSL
open Bistro_bioinfo.Std

module NGS_QC = struct
  type input = {
    sample_id : string ;
    sample_file : string [@file] ;
  }
  [@@deriving sexp, bistro_form]

  let title = "NGS_QC"

  let derive i = Bistro_repo.[
      [ i.sample_id ] %> FastQC.run (input i.sample_file) ;
    ]

end

module Server = Bistro_server.Make(NGS_QC)

let () = Lwt_main.run (Server.start ())
