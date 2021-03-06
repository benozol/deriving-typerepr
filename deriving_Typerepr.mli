
(** {1 Structural representations of types} *)

type 'a atomic =
  | Unit : unit atomic
  | Int : int atomic
  | Bool : bool atomic
  | String : string atomic
  | Float : float atomic
  | Int32 : int32 atomic
  | Int64 : int64 atomic

type 'a t = private
  | Abstract : string -> 'a t
  | Atomic : 'a atomic -> 'a t
  | Tuple : 'a tuple -> 'a t
  | Function : string option * 'a t * 'b t -> ('a -> 'b) t
  | List : 'a t -> 'a list t
  | Option : 'a t -> 'a option t
  | Array : 'a t -> 'a array t
  | Ref : 'a t -> 'a ref t
  | Sum : 'a sum -> 'a t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t

and ('a, 'b) component = private
  | Component : 'b t * int -> ('a, 'b) component
and 'a any_component =
  | Any_component : ('a, 'b) component -> 'a any_component
and 'a tuple = private
  { components : 'a any_component list }

and ('a, 'b) field = private
  | Field : int * 'b t -> ('a, 'b) field
and 'a any_field =
  | Any_field : ('a, 'b) field -> 'a any_field
and 'a record = private
    { fields : (string * 'a any_field) list; }

and ('a, 'b) summand = private
  | Summand_nullary : 'a nullary -> ('a, unit) summand
  | Summand_unary : ('a, 'b) unary -> ('a, 'b) summand
  | Summand_nary : ('a, 'b) nary -> ('a, 'b) summand
and 'a any_summand =
  | Any_summand : ('a, 'b) summand -> 'a any_summand
and 'a sum = private
    { summands : (string * 'a any_summand) list; }

and ('a, 'b) tagspec =
  | Tag_nullary : 'a nullary -> ('a, unit) tagspec
  | Tag_unary : ('a, 'b) unary -> ('a, 'b) tagspec
  | Tag_nary : ('a, 'b) nary -> ('a, 'b) tagspec
and 'a any_tagspec =
  | Any_tagspec : ('a, 'b) tagspec -> 'a any_tagspec
and 'a variant = private
    { tagspecs : (string * 'a any_tagspec) list }

and 'a nullary = private int
and ('a, 'b) unary = private int * 'b t
and ('a, 'b) nary = private int * 'b tuple

type any_t = Any_t : 'a t -> any_t

(** {2 Dynamically values} *)

type dyn =
  | Dyn : 'a t * 'a -> dyn

(** {2 Creation} *)

type 'a create_record_field = {
  create_record_field : 'b. string -> ('a, 'b) field -> 'b;
}
val create_record : 'a record -> 'a create_record_field -> 'a
type 'a create_tuple_component = {
  create_tuple_component : 'b. ('a, 'b) component -> 'b;
}
val create_tuple : 'a tuple -> 'a create_tuple_component -> 'a
val create_sum_case : ('a, 'b) summand -> 'b -> 'a
val create_variant_case : ('a, 'b) tagspec -> 'b -> 'a

(** {2 Inspection} *)

val get_record_field : ('a, 'b) field -> 'a -> 'b
val get_tuple_component : ('a, 'b) component -> 'a -> 'b
type 'a any_case_value =
  | Any_case_value : ('a, 'b) summand * 'b -> 'a any_case_value
val get_sum_case : 'a sum -> 'a -> string * 'a any_case_value
val get_sum_case_by_summand : ('a, 'b) summand ->'a -> 'b option
type 'a any_tagspec_value =
  | Any_variant_value : ('a, 'b) tagspec * 'b -> 'a any_tagspec_value
val get_variant_case : 'a variant -> 'a -> string * 'a any_tagspec_value
val get_variant_case_by_tagspec : ('a, 'b) tagspec ->'a -> 'b option

(** {2 Paths to components} *)

(** A value of type [('a, 'b,) p] encodes the path from a value of
    type ['a] to a component of type ['b]. *)
type ('a, 'b) p =
  | Root : ('a, 'a) p
  | Tuple_component : ('b, 'c) component * ('a, 'b) p -> ('a, 'c) p
  | List_item : int * ('a, 'b list) p -> ('a, 'b) p
  | Array_item : int * ('a, 'b array) p -> ('a, 'b) p
  | Case_nullary : 'b nullary * ('a, 'b) p  -> ('a, unit) p
  | Case_unary : ('b, 'c) unary * ('a, 'b) p  -> ('a, 'c) p
  | Case_nary : ('b, 'c) nary * ('a, 'b) p -> ('a, 'c) p
  | Record_field : ('b, 'c) field * ('a, 'b) p -> ('a, 'c) p
  | Option_some : ('a, 'b option) p -> ('a, 'b) p
  | Ref_content : ('a, 'b ref) p -> ('a, 'b) p
  | Variant_case_nullary : 'b nullary * ('a, 'b) p  -> ('a, unit) p
  | Variant_case_unary : ('b, 'c) unary * ('a, 'b) p  -> ('a, 'c) p
  | Variant_case_nary : ('b, 'c) nary * ('a, 'b) p -> ('a, 'c) p

val get : 'a -> ('a, 'c) p -> 'c option

val compose : ('b, 'c) p -> ('a, 'b) p -> ('a, 'c) p

type ('a, 'acc) folder = {
  folder : 'b . 'acc -> 'b -> 'b t -> ('a, 'b) p  -> 'acc
}

val fold : ('a, 'acc) folder -> 'acc -> 'a -> 'a t -> 'acc

(** {2 Auxiliaries} *)

val show : 'a t -> 'a -> string

module Path : sig

  (** Helpers for path construction
      Useful with the [compose] above. *)

  val root : ('a, 'a) p
  val list_item : int -> ('a list, 'a) p
  val array_item : int -> ('a array, 'a) p
  val some : ('a option, 'a) p
  val content : ('a ref, 'a) p

  (** The following functions raise [Not_found] if the component
      doesn't exist and [Failure _] when the type doesn't match. *)

  val field : 'a t -> string -> 'b t -> ('a, 'b) p
  val case_nullary : 'a t -> string -> ('a, unit) p
  val case_unary : 'a t -> string -> 'b t -> ('a, 'b) p
  val case_nary : 'a t -> string -> 'b t -> ('a, 'b) p
  val component : 'a t -> int -> 'b t -> ('a, 'b) p
end

(** {2 Predefined type representations} *)

module type Typerepr = sig type a val t : a t end

module Typerepr_unit : Typerepr with type a = unit
module Typerepr_int : Typerepr with type a = int
module Typerepr_bool : Typerepr with type a = bool
module Typerepr_int32 : Typerepr with type a = int32
module Typerepr_int64 : Typerepr with type a = int64
module Typerepr_float : Typerepr with type a = float
module Typerepr_string : Typerepr with type a = string
module Typerepr_option : functor (T : Typerepr) -> Typerepr with type a = T.a option
module Typerepr_list : functor (T : Typerepr) -> Typerepr with type a = T.a list
module Typerepr_array : functor (T : Typerepr) -> Typerepr with type a = T.a array
module Typerepr_ref : functor (T : Typerepr) -> Typerepr with type a = T.a ref

val abstract : string -> 'a t

(**/**)

val eq : 'a t -> 'b t -> bool
val eq_atomic : 'a atomic -> 'b atomic -> bool

(** Untyped constructors *)

val __field__ : int -> 'b t -> ('a, 'b) field
val __record__ : (string * 'a any_field) list -> 'a t
val __summand_nullary__ : int -> ('a, unit) summand
val __summand_unary__ : int -> 'b t -> ('a, 'b) summand
val __summand_nary__ : int -> 'b any_component list -> ('a, 'b) summand
val __component__ : int -> 'b t -> ('a, 'b) component
val __tuple__ : 'a any_component list -> 'a t
val __sum__ : (string * 'a any_summand) list -> 'a t
val __tag_nullary__ : int -> ('a, unit) tagspec
val __tag_unary__ : int -> 'b t -> ('a, 'b) tagspec
val __tag_nary__ : int -> 'b any_component list -> ('a, 'b) tagspec
val __variant__ : (string * 'a any_tagspec) list -> 'a t
