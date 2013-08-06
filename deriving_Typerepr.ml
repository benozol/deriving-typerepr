
external (@) : ('a -> 'b) -> 'a -> 'b = "%apply"
let (@@) = List.append
let flip f x y = f y x
external identity : 'a -> 'a = "%identity"
module Option = struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None
  let bind f = function
    | Some x -> f x
    | None -> None
end
let list_index xs x =
  let rec aux ix = function
    | [] -> raise Not_found
    | x' :: _ when x = x' -> ix
    | _ :: xs -> aux (succ ix) xs
  in
  aux 0 xs

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
  | List : 'a t -> 'a list t
  | Option : 'a t -> 'a option t
  | Array : 'a t -> 'a array t
  | Ref : 'a t -> 'a ref t
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
      | Summand_nullary ix when Obj.is_int r && ix = Obj.obj r ->
        Some ()
      | Summand_unary (ix, _) when Obj.is_block r && ix = Obj.tag r ->
        Some (Obj.obj @ Obj.field r 0)
      | Summand_nary (ix, _) when Obj.is_block r && ix = Obj.tag r ->
        let o = Obj.dup r in
        Obj.set_tag o ix;
        Some (Obj.obj o)
      | _ -> None

let create_sum_case : type a b . (a, b) summand -> b -> a =
  fun summand arg ->
    let r = Obj.repr arg in
    match summand with
      | Summand_nullary ix ->
        Obj.magic ix
      | Summand_unary (ix, t) ->
        let o = Obj.new_block ix 1 in
        Obj.set_field o 0 r;
        Obj.obj o
      | Summand_nary (ix, { components}) ->
        let o = Obj.dup r in
        Obj.set_tag o ix;
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


type ('w, 'a, 'b, 'c) p =
  | Root : ('w, 'a, 'w, 'a) p
  | Tuple_component : ('b, 'c) component * ('w, 'a, _, 'b) p -> ('w, 'a, 'b, 'c) p
  | List_item : int * ('w, 'a, _, 'c list) p -> ('w, 'a, 'c list, 'c) p
  | Array_item : int * ('w, 'a, _, 'c array) p -> ('w, 'a, 'c array, 'c) p
  | Case_unary : ('b, 'c) unary_summand * ('w, 'a, _, 'b) p  -> ('w, 'a, 'b, 'c) p
  | Case_nary : ('d, 'c) component * ('b, 'd) nary_summand * ('w, 'a, _, 'b) p -> ('w, 'a, 'b, 'c) p
  | Record_field : ('b, 'c) field * ('w, 'a, _, 'b) p -> ('w, 'a, 'b, 'c) p
  | Option_some : ('w, 'a, _, 'c option) p -> ('w, 'a, 'c option, 'c) p
  | Ref_content : ('w, 'a, _, 'c ref) p -> ('w, 'a, 'c ref, 'c) p

let rec get : type w a b c . a -> (w, a, b, c) p -> c option =
  fun value path ->
    match path with
      | Tuple_component (component, path) ->
        Option.map
          (get_tuple_component component)
          (get value path)
      | List_item (ix, path) ->
        Option.map
          (flip List.nth ix)
          (get value path)
      | Array_item (ix, path) ->
        Option.map
          (flip Array.get ix)
          (get value path)
      | Case_unary (unary_summand, path) ->
        Option.bind
          (get_sum_case_by_summand (Summand_unary unary_summand))
          (get value path)
      | Case_nary (component, (_, { components } as nary_summand), path) ->
        Option.map
          (get_tuple_component component)
          (Option.bind
             (get_sum_case_by_summand (Summand_nary nary_summand))
             (get value path))
      | Record_field (field, path) ->
        Option.map
          (get_record_field field)
          (get value path)
      | Option_some path ->
        Option.bind identity
          (get value path)
      | Ref_content path ->
        Option.map
          (fun ref -> !ref)
          (get value path)
      | Root -> Some value

let rec compose : type w1 a1 a2 a3 b2 b3 . (w1, a1, b2, a2) p -> (b2, a2, b3, a3) p -> (w1, a1, b3, a3) p =
  fun pos1 pos2 ->
    match pos2 with
      | Tuple_component (component, pos2) ->
        let pos3 = compose pos1 pos2 in
        Tuple_component (component, pos3)
      | List_item (ix, pos2) ->
        let pos3 = compose pos1 pos2 in
        List_item (ix, pos3)
      | Option_some pos2 ->
        let pos3 = compose pos1 pos2 in
        Option_some pos3
      | Array_item (ix, pos2) ->
        let pos3 = compose pos1 pos2 in
        Array_item (ix, pos3)
      | Case_unary (summand, pos2) ->
        let pos3 = compose pos1 pos2 in
        Case_unary (summand, pos3)
      | Case_nary (component, summand, pos2) ->
        let pos3 = compose pos1 pos2 in
        Case_nary (component, summand, pos3)
      | Record_field (field, pos2) ->
        let pos3 = compose pos1 pos2 in
        Record_field (field, pos3)
      | Ref_content pos2 ->
        let pos3 = compose pos1 pos2 in
        Ref_content pos3
      | Root -> pos1

type ('w, 'a, 'acc) folder = {
  folder : 'c 'b . 'acc -> 'c -> 'c t -> ('w, 'a, 'b, 'c) p  -> 'acc
}

let fold f init value t =
  let rec aux : type b c . 'acc -> c t -> c -> ('w, 'root, b, c) p -> 'acc =
    fun sofar t value path ->
      let sofar =
        match t with
          | Atomic _ -> sofar
          | Tuple { components } ->
            let folder sofar (Any_component (Component (t, ix) as component)) =
              let value = get_tuple_component component value in
              aux sofar t value @
                Tuple_component (component, path)
            in
            List.fold_left folder sofar components
          | Option t -> begin
              match value with
                | None -> sofar
                | Some value ->
                  let path = Option_some path in
                  aux sofar t value path
            end
          | List t ->
            let folder sofar (ix, value) =
              aux sofar t value @
                List_item (ix, path)
            in
            List.fold_left folder sofar @
              flip List.mapi value @
                fun ix value -> ix, value
          | Array t ->
            let folder sofar (ix, value) =
              aux sofar t value @
                Array_item (ix, path)
            in
            Array.fold_left folder sofar @
              flip Array.mapi value @
                fun ix value -> ix, value
          | Ref t ->
            aux sofar t !value @
              Ref_content path
          | Sum sum ->
            let name, Any_case_value (summand, value) = get_sum_case sum value in begin
              match summand with
                | Summand_nullary ix ->
                  sofar
                | Summand_unary unary_summand ->
                  let _, t = (unary_summand : (_, _) unary_summand :> _ * _) in
                  aux sofar t value @
                    Case_unary (unary_summand, path)
                | Summand_nary nary_summand ->
                  let _, { components } = (nary_summand : (_,_) nary_summand :> _ * _) in
                  let folder sofar (Any_component (Component (t, ix) as component)) =
                    let value = get_tuple_component component value in
                    aux sofar t value @
                      Case_nary (component, nary_summand, path)
                  in
                  List.fold_left folder sofar components
            end
          | Record { fields } ->
            let folder sofar (name, Any_field (Field (_, t) as field)) =
              let value = get_record_field field value in
              aux sofar t value @
                Record_field (field, path)
            in
            List.fold_left folder sofar fields
          | Function _ -> failwith "Deriving_Typerepr.fold"
    in
    f.folder sofar value t path
  in
  aux init t value Root
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
