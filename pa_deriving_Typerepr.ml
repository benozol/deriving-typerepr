
open Pa_deriving_common
(* open Utils *)

external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (@@) = List.append
let flip f x y = f y x
let flip2 f x y z = f z x y
let option_map f = function
  | None -> None
  | Some x -> Some (f x)

module Description : Defs.ClassDescription = struct
  let classname = "Typerepr"
  let runtimename = "Deriving_Typerepr"
  let default_module = None
  let alpha = None
  let allow_private = true
  let predefs = [
    ["int" ], ["Deriving_Typerepr";"int"];
    ["bool" ], ["Deriving_Typerepr";"bool"];
    ["unit" ], ["Deriving_Typerepr";"unit"];
    ["char" ], ["Deriving_Typerepr";"char"];
    ["int32" ], ["Deriving_Typerepr";"int32"];
      (* ["Int32";"t"], ["Deriving_Typerepr";"int32"]; *)
    ["int64" ], ["Deriving_Typerepr";"int64"];
      (* ["Int64";"t"], ["Deriving_Typerepr";"int64"]; *)
      ["nativeint"], ["Deriving_Typerepr";"nativeint"];
    ["float" ], ["Deriving_Typerepr";"float"];
      (* ["num" ], ["Deriving_num" ;"num"]; *)
    ["string" ], ["Deriving_Typerepr";"string"];
    ["list" ], ["Deriving_Typerepr";"list"];
    ["ref" ], ["Deriving_Typerepr";"ref"];
    ["option" ], ["Deriving_Typerepr";"option"];
    ["array" ], ["Deriving_Typerepr";"array"];
  ]
  let depends = []
end

module Builder (Loc : Defs.Loc) = struct

  module Helpers = Base.AstHelpers(Loc)
  module Generator = Base.Generator(Loc)(Description)

  open Loc
  open Camlp4.PreCast

  let wrap_params params =
    Helpers.expr_list (List.map (fun str -> <:expr< $`str:str$ >>) params)

  (* let wrap_fresh ctxt ?eq params repr = *)
  (*   let eq = *)
  (*     match eq with *)
  (*       | None -> <:expr< None >> *)
  (*       | Some eq -> *)
  (*         <:expr< Some $eq$ >> *)
  (*   in *)
  (*   [ <:str_item< *)
  (*       let t = { *)
  (*         params = $wrap_params params$ ; *)
  (*         rhs = `Fresh ($eq$, $repr$); *)
  (*       } *)
  (*     >> ] *)

  (* let wrap_expr ctxt expr = *)
  (*   [ <:str_item< *)
  (*       let t = { *)
  (*         params = $wrap_params [] (\* params *\)$ ; *)
  (*         rhs = `Expr expr *)
  (*       } *)
  (*     >> ] *)

  let generator = (object (self)

    inherit Generator.generator

    method proxy () =
      None, [ <:ident< ty >> ]

    method tuple ctxt args =
      let exprs = List.map (fun arg -> self#call_expr ctxt arg "t") args in
      [ <:str_item< let t : Deriving_Typerepr.expr = `Tuple ($Helpers.expr_list exprs$) >> ]

    method record ?eq ctxt name args constraints fields =
      (* let eq = flip option_map eq @ fun eq -> self#call_expr ctxt eq "t" in *)
      let fields =
        flip List.map fields @ fun (name, (vars, expr), mutability) ->
          let expr = self#call_expr ctxt expr "t" in
          <:expr< $`str:name$, ($wrap_params (List.map fst vars)$, $expr$) >>
      in
      let args = flip List.map args @ fun arg -> self#call_expr ctxt arg "t" in
      [ <:str_item< let t : Deriving_Typerepr.expr = `Record $Helpers.expr_list fields$ >> ]

    method sum ?eq ctxt name args constraints summands =
      (* let eq = flip option_map eq @ fun eq -> self#call_expr ctxt eq "t" in *)
      let summands =
        flip List.map summands @ fun (name, exprs) ->
          let exprs = flip List.map exprs @ fun expr ->
            self#call_expr ctxt expr "t"
          in
          <:expr< $`str:name$, $Helpers.expr_list exprs$ >>
      in
      let args = flip List.map args @ fun arg -> self#call_expr ctxt arg "t" in
      [ <:str_item< let t : Deriving_Typerepr.expr = `Sum $Helpers.expr_list summands$ >> ]

    method variant = Base.fatal_error _loc "Variants not yet supported"
    (* method class_ = Base.fatal_error _loc "Classes not yet supported" *)
    (* method object_ = Base.fatal_error _loc "Objects not yet supported" *)
    (* method label = Base.fatal_error _loc "Objects not yet supported" *)

    (* method function_ ctxt (expr, expr') = *)
    (*   let expr = self#call_expr ctxt expr "t" in *)
    (*   let expr' = self#call_expr ctxt expr' "t" in *)
    (*    [ <:str_item< let t = `Function ($expr$, $expr'$) >> ] *)

  end :> Generator.generator)

  let classname = Description.classname
  let runtimename = Description.runtimename
  let generate = Generator.generate generator
  let generate_sigs = Generator.generate_sigs generator
  let generate_expr = Generator.generate_expr generator

end


module Typerepr = Base.Register(Description)(Builder)

let depends = (module Builder : Defs.FullClassBuilder)
