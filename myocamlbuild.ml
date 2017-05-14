open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "bistroserver"
let version = "dev"

let annot = ()
let bin_annot = ()
let g = ()
let short_paths = ()
let thread = ()

let undash = String.map (function '-' -> '_' | c -> c)

let clientapp =
  let name = "bistro_client" in
  Project.app name
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~file:(sprintf "app/%s_app.ml" (undash name))
    ~findlib_deps:[
      "js_of_ocaml" ;
      "js_of_ocaml.ppx" ;
    ]

let lib =
  let name = "bistroserver" in
  Project.lib name
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~install:(`Findlib name)
    ~dir:"lib"
    ~style:`Basic
    ~ml_files:(`Add ["bistro_server_js.ml"])
    ~findlib_deps:[
      "cohttp.lwt" ;
      "tyxml"
    ]

let test1 =
  let name = "test1" in
  Project.app name
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~file:"app/test1.ml"
    ~internal_deps:[lib]

let items = [ test1 ; clientapp ; lib ;  ]


let () =
  let open Solvuu_build.Std.Project in

  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let libs = filter_libs items in
  let apps = filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      Solvuu_build.Util.Rule.rule
        ~prods:["lib/bistro_server_js.ml"]
        ~deps:["app/bistro_client.js"]
        Ocamlbuild_plugin.(fun env _ ->
           let arg = env "app/bistro_client.js" in
           let out = env "lib/bistro_server_js.ml" in
           let l = [A"ocamlify" ; A"--var-string" ; A"contents" ; P arg ; Sh ">" ; Px out] in
           Cmd (S l)) ;

      Solvuu_build.Util.Rule.rule
        ~prods:["app/bistro_client.js"]
        ~deps:["app/bistro_client.byte"]
        Ocamlbuild_plugin.(fun env _ ->
           let arg = env "app/bistro_client.byte" in
           let l = [A"js_of_ocaml" ; P arg] in
           Cmd (S l)) ;

      build_static_file ".merlin" (fun () -> merlin_file items);
      build_static_file ".ocamlinit" (fun () -> ocamlinit_file items);
      build_static_file "project.mk" (fun () -> makefile items ~project_name);
      (
        match meta_file ~version libs with
        | Some x -> Findlib.build_meta_file x
        | None -> ()
      );
      build_static_file (sprintf "%s.install" project_name)
        (fun () -> install_file items);
    )
  | _ -> ()
