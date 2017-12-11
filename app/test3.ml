open Base
open Bistro.EDSL
open Bistro_bioinfo.Std
open Bistro_utils

module ChIP_seq_pipeline = struct
  type input = {
    sra_identifier : string ;
    genome : string ;
    (* macs2_qvalue_threshold : float ; *)
    number_of_motifs : int ;
  }
  [@@deriving sexp, bistro_form]

  let title = "ChIP-seq pipeline"

  let derive ~data i =
    let sample_sra = Sra.fetch_srr i.sra_identifier in
    let sample_fq = Sra_toolkit.fastq_dump sample_sra in
    let org = Option.value_exn (Ucsc_gb.genome_of_string i.genome) in
    let genome = Ucsc_gb.genome_sequence org in
    let index = Bowtie.bowtie_build genome in
    let mapped_reads =
      Bowtie.bowtie ~v:2 index (`single_end [sample_fq]) in
    let peaks =
      Macs2.(callpeak ~extsize:150 ~nomodel:true
               (* ~qvalue:i.macs2_qvalue_threshold *)
               sam [ mapped_reads ] / narrow_peaks)
    in
    let genome_2bit = Ucsc_gb.genome_2bit_sequence org in
    let sequences =
      Ucsc_gb.twoBitToFa
        Ucsc_gb.(bedClip (fetchChromSizes `sacCer2) (Bed.keep4 peaks))
        genome_2bit
    in
    let motifs = Meme_suite.meme_chip ~meme_nmotifs:i.number_of_motifs sequences in
    Repo.[
      [ "QC" ] %> FastQC.run sample_fq ;
      [ "peaks" ] %> peaks ;
      [ "motifs" ] %> motifs
    ]
end

module Server = Bistro_server.Make(ChIP_seq_pipeline)
let () = Lwt_main.run (Server.start ())

