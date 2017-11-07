exception Unimplemented

module type Formattable = sig
  type t
  val format : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t
  val compare : t -> t -> [ `EQ | `GT | `LT ]
  include Formattable with type t := t
end

type ('k,'v) tree23 =
  | Leaf
  | Twonode of ('k,'v) twonode
  | Threenode of ('k,'v) threenode
and ('k,'v) twonode = {
  left2  : ('k,'v) tree23;
  value  : 'k * 'v;
  right2 : ('k,'v) tree23;
}
and ('k,'v) threenode = {
  left3   : ('k,'v) tree23;
  lvalue  : 'k * 'v;
  middle3 : ('k,'v) tree23;
  rvalue  : 'k * 'v;
  right3  : ('k,'v) tree23;
}

module type Dictionary = sig
  module Key : Comparable
  module Value : Formattable
  type key = Key.t
  type value = Value.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : key -> value -> t -> t
  val member : key -> t -> bool
  val find : key -> t -> value option
  val remove : key -> t -> t
  val choose : t -> (key * value) option
  val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> (key * value) list
  val expose_tree : t -> (key,value) tree23
  val format : Format.formatter -> t -> unit
end

module type DictionaryMaker =
  functor (K : Comparable) (V : Formattable)
    -> Dictionary with module Key = K and module Value = V

module MakeListDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V
  type key = K.t
  type value = V.t

  (* TODO: change type [t] from [unit] to something involving
   * association lists. *)
  (* AF: TODO: document the abstraction function.
   * RI: TODO: document any representation invariants. *)
  type t = unit

  let rep_ok d =
    raise Unimplemented

  let empty = raise Unimplemented

  let is_empty d =
    raise Unimplemented

  let size d =
    raise Unimplemented

  let insert k v d =
    raise Unimplemented

  let remove k d =
    raise Unimplemented

  let find k d =
    raise Unimplemented

  let member k d =
    raise Unimplemented

  let choose d =
    raise Unimplemented

  let to_list d =
    raise Unimplemented

  let fold f init d =
    raise Unimplemented

  let expose_tree d =
    failwith "not a 2-3 tree"

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

module MakeTreeDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V
  type key = K.t
  type value = V.t

  type t = (key,value) tree23

  let rep_ok d =
    raise Unimplemented

  let empty = raise Unimplemented

  let is_empty d =
    raise Unimplemented

  let size d =
    raise Unimplemented

  let insert k v d =
    raise Unimplemented

  let remove k d =
    raise Unimplemented

  let find k d =
    raise Unimplemented

  let member k d =
    raise Unimplemented

  let choose d =
    raise Unimplemented

  let to_list d =
    raise Unimplemented

  let expose_tree d =
    d

  let fold f init d =
    raise Unimplemented

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

module type Set = sig
  module Elt : Comparable
  type elt = Elt.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module MakeSetOfDictionary (C : Comparable) (DM:DictionaryMaker) = struct
  module Elt = C
  type elt = Elt.t

  (* TODO: change type [t] to something involving a dictionary *)
  type t = unit

  let rep_ok s =
    raise Unimplemented

  let empty =
    raise Unimplemented

  let is_empty s =
    raise Unimplemented

  let size s =
    raise Unimplemented

  let insert x s =
    raise Unimplemented

  let member x s =
    raise Unimplemented

  let remove x s =
    raise Unimplemented

  let choose s =
    raise Unimplemented

  let fold f init s =
    raise Unimplemented

  let union s1 s2 =
    raise Unimplemented

  let intersect s1 s2 =
    raise Unimplemented

  let difference s1 s2 =
    raise Unimplemented

  let to_list s =
    raise Unimplemented

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end
