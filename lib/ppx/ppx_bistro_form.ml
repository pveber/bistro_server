open Ppx_core
open Ast_builder.Default

let ( @@ ) = Caml.( @@ )

module Type_conv = Ppx_type_conv.Std.Type_conv

module Str = struct
  let form_of_td td ~rec_flag =
    let td = name_type_params_in_td td in
    let loc = td.ptype_loc in
    let pat = pvar ~loc "form" in
    let body = eint ~loc 42 in
    value_binding ~loc ~pat ~expr:body

  let form_of_tds ~loc ~path (rec_flag, tds) =
    let bindings = List.map tds ~f:(form_of_td ~rec_flag) in
    pstr_value_list ~loc rec_flag bindings

end

module Sig = struct

  let form_of_tds ~loc ~path (_, tds) =
    List.map tds ~f:(fun td ->
        let loc = td.ptype_loc in
        psig_value ~loc @@ value_description ~loc
          ~name:(Located.map (Fn.flip (^) "_bistro_form") td.ptype_name)
          ~type_:[%type: Bistro_server_common.form]
          ~prim:[]
      )
end

let str_type_decl =
  Type_conv.Generator.make_noarg Str.form_of_tds ~attributes:[]

let sig_type_decl =
  Type_conv.Generator.make_noarg Sig.form_of_tds ~attributes:[]

let bistro_form = Type_conv.add "bistro_form" ~str_type_decl ~sig_type_decl
