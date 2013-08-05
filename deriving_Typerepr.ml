
type 'a atomic =
  | Unit : unit atomic
  | Int : int atomic
  | Bool : bool atomic
  | String : string atomic
  | Float : float atomic
  | Int32 : int32 atomic
  | Int64 : int64 atomic

type 'a t =
  | Atomic : 'a atomic -> 'a t
  | Tuple : 'a tuple -> 'a t
  | Function : string option * 'a t * 'b t -> ('a -> 'b) t
  | Ref : 'a t -> 'a ref t
  | Option : 'a t -> 'a option t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Sum : 'a sum -> 'a t
  | Record : 'a record -> 'a t

and ('a, 'b) field = Field : int * 'b t -> ('a, 'b) field
and 'a any_field = Any_field : ('a, _) field -> 'a any_field
and 'a record = { fields : (string * 'a any_field) list }

and ('a, 'b) summand =
  | Summand_nullary : int -> ('a, unit) summand
  | Summand_unary : ('a, 'b) unary_summand -> ('a, 'b) summand
  | Summand_nary : ('a, 'b) nary_summand -> ('a, 'b) summand
and ('a, 'b) unary_summand = int * 'b t
and ('a, 'b) nary_summand = int * 'b tuple
and 'a any_summand = Any_summand : ('a, _) summand -> 'a any_summand
and 'a sum = { summands : (string * 'a any_summand) list }

and ('a, 'b) component = Component : 'b t * int -> ('a, 'b) component
and 'a any_component = Any_component : ('a, _) component -> 'a any_component
and 'a tuple = { components : 'a any_component list }

type dyn = Dyn : 'a t * 'a -> dyn
type dyn_tuple = Dyn_tuple : 'a tuple * 'a -> dyn_tuple
type any_t = Any_t : 'a t -> any_t

external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (@@) = List.append
let flip f x y = f y x

let list_index xs x =
  let rec aux ix = function
    | [] -> raise Not_found
    | x' :: _ when x = x' -> ix
    | _ :: xs -> aux (succ ix) xs
  in
  aux 0 xs

let get_record_field : type a b . (a, b) field -> a -> b =
  fun field value ->
    match field with
      | Field (ix, t) ->
        let field_value = Obj.obj @ Obj.field (Obj.repr value) ix in
        field_value

let get_record_fields : type a . a record -> a -> (string * dyn) list =
  fun { fields } value ->
    flip List.map fields @ fun (name, Any_field (Field (_, t) as field)) ->
      let value = get_record_field field value in
      name, Dyn (t, value)

type 'a create_record_field = {
  create_record_field : 'b . string -> ('a, 'b) field -> 'b;
}

let create_record : type a . a record -> a create_record_field -> a =
  fun { fields } f ->
    let o = Obj.new_block 0 (List.length fields) in
    begin
      flip List.iter fields @ fun (field_name, Any_field (Field (ix, _) as field)) ->
        Obj.set_field o ix @ Obj.repr @
          f.create_record_field field_name field
    end;
    Obj.obj o

let get_tuple_component : type a b . (a, b) component -> a -> b =
  fun component value ->
    match component with
      | Component (t, ix) ->
          Obj.obj @ Obj.field (Obj.repr value) ix

let get_tuple_components : 'a . 'a any_component list -> 'a -> dyn list =
  fun (type a) (components : a any_component list) value ->
    flip List.map components @
      fun (type b) (Any_component component) ->
        let Component (t, _) = component in
        Dyn (t, get_tuple_component component value)

type 'a create_tuple_component = {
  create_tuple_component : 'b . ('a, 'b) component -> 'b;
}
let create_tuple : type a . a any_component list -> a create_tuple_component -> a =
  fun components f ->
    let o = Obj.new_block 0 (List.length components) in
    begin
      flip List.iteri components @ fun ix (Any_component component) ->
        Obj.set_field o ix
          (Obj.repr @ f.create_tuple_component component)
    end;
    Obj.obj o

let get_sum_case_by_summand : type a b . (a, b) summand -> a -> b option =
  fun summand v ->
    let r = Obj.repr v in
    match summand with
      | Summand_nullary ix ->
        if Obj.is_int r && ix = Obj.obj r then
          Some ()
        else
          None
      | Summand_unary (ix, t) ->
        if Obj.is_block r then
          Some (Obj.obj @ Obj.field r 0)
        else
          None
      | Summand_nary (ix, { components }) ->
        if Obj.is_block r then
          let o = Obj.new_block 0 @ List.length components in
          (flip List.iter components @ fun (Any_component component) ->
            let Component (t, ix) = component in
            Obj.set_field o ix (Obj.field r ix));
          Some (Obj.obj o)
        else
          None

let create_sum_case : type a b . (a, b) summand -> b -> a =
  fun summand arg ->
    match summand with
      | Summand_nullary ix ->
        Obj.magic ix
      | Summand_unary (ix, t) ->
        let o = Obj.new_block ix 1 in
        Obj.set_field o 0 (Obj.repr arg);
        Obj.obj o
      | Summand_nary (ix, { components}) ->
        let o = Obj.new_block ix (List.length components) in
        begin
          flip List.iter components @ fun (Any_component component) ->
            let Component (t, ix) = component in
            Obj.set_field o ix (Obj.field (Obj.repr arg) ix)
        end;
        Obj.obj o

type 'a any_case_value =
  | Any_case_value : ('a, 'b) summand * 'b -> 'a any_case_value

let get_sum_case : type a b . a sum -> a -> string * a any_case_value =
  fun (type a) { summands } value ->
    let rec aux = function
      | [] -> assert false
      | (name, Any_summand summand) :: summands ->
        match get_sum_case_by_summand summand value with
          | None -> aux summands
          | Some value -> name, Any_case_value (summand, value)
    in
    aux summands

let rec show : type a . a t -> a -> string =
  fun t value ->
    match t with
      | Atomic Unit -> "()"
      | Atomic Bool -> if value then "true" else "false"
      | Atomic Int -> string_of_int value
      | Atomic Int32 -> Int32.to_string value
      | Atomic Int64 -> Int64.to_string value
      | Atomic Float -> string_of_float value
      | Atomic String -> value
      | Tuple { components } ->
        let ss =
          flip List.map (get_tuple_components components value) @ fun dyn ->
            let Dyn (t, value) = dyn in
            show t value
        in
        "("^String.concat ", " ss^")"
      | Function _ -> failwith "<abstract>"
      | Option t ->
        (match value with
        | None -> "None"
        | Some value -> "Some ("^show t value^")")
      | List t ->
        let ss =
          flip List.map value @ show t
        in
        "["^String.concat "; " ss^"]"
      | Array t ->
        let ss =
          flip List.map (Array.to_list value) @ show t
        in
        "[|"^String.concat "; " ss^"|]"
      | Ref t ->
        "ref ("^show t !value^")"
      | Record record ->
        let ss =
          flip List.map (get_record_fields record value) @ fun (name, dyn) ->
            let Dyn (t, value) = dyn in
            name^": "^show t value
        in
        "{"^String.concat "; " ss^"}"
      | Sum sum ->
        let name, any_case_value = get_sum_case sum value in
        match any_case_value with
          | Any_case_value (Summand_nullary _, ()) -> name
          | Any_case_value (Summand_unary (_, t), value) -> show t value
          | Any_case_value (Summand_nary (_, tuple), value) ->
            name^" "^show (Tuple tuple) value

module type Typerepr =
sig
  type a
  val t : a t
end
module Typerepr_unit = struct type a = unit let t = Atomic Unit end
module Typerepr_int = struct type a = int let t = Atomic Int end
module Typerepr_bool = struct type a = bool let t = Atomic Bool end
module Typerepr_int32 = struct type a = int32 let t = Atomic Int32 end
module Typerepr_int64 = struct type a = int64 let t = Atomic Int64 end
module Typerepr_float = struct type a = float let t = Atomic Float end
module Typerepr_string = struct type a = string let t = Atomic String end
module Typerepr_option (T : Typerepr) : Typerepr with type a = T.a option = struct
  type a = T.a option
  let t = Option T.t
end
module Typerepr_list (T : Typerepr) : Typerepr with type a = T.a list = struct
  type a = T.a list
  let t = List T.t
end
module Typerepr_array (T : Typerepr) : Typerepr with type a = T.a array = struct
  type a = T.a array
  let t = Array T.t
end
module Typerepr_ref (T : Typerepr) : Typerepr with type a = T.a ref = struct
  type a = T.a ref
  let t = Ref T.t
end

let __field__ ix t = Field (ix, t)
let __record__ fields = Record { fields }
let __summand_nullary__ ix = Summand_nullary ix
let __summand_unary__ ix t = Summand_unary (ix, t)
let __summand_nary__ ix components = Summand_nary (ix, { components })
let __sum__ summands = Sum { summands }
let __component__ ix t = Component (t, ix)
let __tuple__ components = Tuple { components }
let __atomic__ atomic = Atomic atomic
