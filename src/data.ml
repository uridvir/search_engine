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
  | Twonode of ('k, 'v) twonode
  | Threenode of ('k, 'v) threenode

and ('k,'v) twonode = {
  left2  : ('k, 'v) tree23;
  value  : 'k * 'v;
  right2 : ('k, 'v) tree23;
}

and ('k,'v) threenode = {
  left3   : ('k, 'v) tree23;
  lvalue  : 'k * 'v;
  middle3 : ('k, 'v) tree23;
  rvalue  : 'k * 'v;
  right3  : ('k, 'v) tree23;
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

  (*
  The dictionary type, essentially an association list (a list of tuples). Each tuple contains a key (of type Key.t), and a value (of type Value.t).
  *)
  type t = (key * value) list

  (*Makes sure this dictionary is valid. Is currently IMPROPERLY IMPLEMENTED and performs no checks.*)
  let rep_ok d : t = d

  (*A variable representing an empty list of type t *)
  let empty : t = []

  (*Checks if a dictionary d is empty by comparing to the empty variable.*)
  let is_empty d : bool = (d = empty)

  (*Runs rep_ok and then returns the number of key-value pairs in the dictionary. It does this by getting the length of the list of tuples.*)
  let size d : int =
    if rep_ok d = d then
      List.length d
    else
      failwith "An exception should have already been thrown, dumbass."

  (*
  Removes all values bound to key k from dictionary. The anonymous function checks if the key of 'a' (k1) does not equal k. The filter function returns a new dictionary of only the
  keys that do NOT equal k (and their associated values).
  *)
  let remove k d : t =
    List.filter (fun a -> let k1, _ = a in Key.compare k k1 != `EQ) d 

  (*
  Inserts value v with key k into dictionary d and returns resulting dictionary. First, uses remove function to remove all previous keys bound to k Finally, appends tuple of the key
  and value inputted to the dictionary.
  *)
  let insert k v d : t =
    let d = remove k d in
    d @ [(k, v)]

  (*
  Finds if key k is bound in dictionary. The anonymous function checks if the key of 'a' (k1) equals k. The exists function checks if any pair satisfies the condition.
  *)
  let member k d : bool =
    List.exists (fun a -> let k1, _ = a in Key.compare k k1 = `EQ) d

  (*
  Finds the value bound to the key k in dictionary. First it checks that the key is in the dictionary. If not, it returns None. Otherwise: The anonymous function here checks if the key
  of the tuple equals k.
  *)
  let find k d : value option =
    if member k d then
      let _, v = List.find (fun a -> let k1, _ = a in Key.compare k k1 = `EQ) d in 
      Some v
    else
      None

  (* Takes in a dictionary, returns the first element if it isn't empty *)
  let choose d : (key * value) option =
    if is_empty d then None else Some (List.hd d)

  (*
  Since d is already an association list, it just sorts it. The anonymous function sorts the keys, and the sort function utilizes the anonymous function to sort the association list.
  *)
  let to_list d : t =
    List.sort (fun a b -> let k, _ = a and k1, _ = b in match Key.compare k k1 with | `LT -> -1 | `EQ -> 0 | `GT -> 1) d

  let fold (f: (key -> value-> 'acc -> 'acc)) (init: 'acc) (d: t) : 'acc =
    List.fold_left (fun init a -> let (key, value) = a in f key value init) init d

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

  type t = (key, value) tree23

  (*TODO: Uri, code this*)
  let rep_ok d =
    raise Unimplemented

  let empty = Leaf

  let is_empty d =
    d = Leaf

  (*TODO: Avi, code this*)
  let size d =
    raise Unimplemented

  (*
   * AVI PLEASE READ:
   * 
   * I found out how to pattern match these 2-3 trees, it's like this. For example, a match on a Twonode might look like:
   *
   *    match d with
   *    | {left2 = left; value = (k1, v1); right2 = right} when left != Leaf && right != Leaf
   *
   * The idea is that a pattern match for a record is supposed to look like declaring a record. (Relevant Stack Overflow question: 
   * https://stackoverflow.com/questions/17173690/record-type-pattern-matching-in-ocaml) The when keyword is something I found out about
   * looking at the OCaml documentation (I won't bore you with that): you just throw it at the end of a pattern match case and the match will
   * only complete if the condition is true. Here, I check if both left and right aren't leaves.
   *
   * Hope it was helpful.
   *)

  (*TODO: Uri, code this*)
  let insert k v d =
    (* Commented out to allow code to compile. TODO: Finish
    let rec climb k v d =
      match d with
      (*two node with two node child on the left being filled*)
      | {left2 = {left2 = Leaf; value = (k2, v2); right2 = Leaf}; value = (k1, v2); right2 = right} when Key.compare k k1 = `LT && Key.compare k k2 = `LT ->
          {left2 = {left3 = Leaf; lvalue = (k, v); middle3 = Leaf; rvalue = (k2, v2); right3 = Leaf}; value2 = (k1, v2); right2 = right}
      | {left2 = {left2 = Leaf; value = (k2, v2); right2 = Leaf}; value = (k1, v2); right2 = right} when Key.compare k k1 = `LT && Key.compare k k2 != `LT ->
          {left2 = {left3 = Leaf; lvalue = (k2, v2); middle3 = Leaf; rvalue = (k, v); right3 = Leaf}; value2 = (k1, v2); right2 = right}
      (*two node with two node child on the right being filled*)
      | Twonode {left; (k1, v2); Twonode {Leaf; (k2, v2); Leaf}} when Key.compare k k1 != `LT && Key.compare k k2 = `LT ->
          Twonode {left; (k1, v2); Threenode {Leaf; (k, v); Leaf; (k2, v2); Leaf}}
      | Twonode {left; (k1, v2); Twonode {Leaf; (k2, v2); Leaf}} when Key.compare k k1 != `LT && Key.compare k k2 != `LT ->
          Twonode {left; (k1, v2); Threenode {Leaf; (k2, v2); Leaf; (k, v); Leaf}}
      (*two node with child on the left to explore*)
      | Twonode {left; (k1, v1); right} when Key.compare k k1 = `LT && left != Leaf ->
          Twonode {climb k v left; (k1, v1); right}
      (*two node with child on the right to explore*)
      | Twonode {left; (k1, v1); right} when Key.compare k k1 != `LT && right != Leaf -> 
          Twonode {left; (k1, v1); climb k v right}
      (*three node with child on the left to explore*)
      | Threenode {left; (k1, v1); middle; (k2, v2); right} when Key.compare k k1 = `LT ->
          Threenode {climb k v left; (k1, v1); middle; (k2, v2); right}
      (*three node with child in the middle to explore*)
      | Threenode {left; (k1, v1); middle; (k2, v2); right} when Key.compare k k2 = `LT -> 
          Threenode {left; (k1, v1); climb k v middle; (k2, v2); right}
      (*three node with child on the right to explore*)
      | Threenode {left; (k1, v1); middle; (k2, v2); right} when Key.compare k k2 != `LT -> 
          Threenode {left; (k1, v1); middle; (k2, v2); climb k v right}
      (*"I want to explore your child." Sounds like something a pedophile would say...*)
    in 
    climb k v d
    *)
    raise Unimplemented
          
  (*TODO: Uri, code this*)
  let remove k d =
    raise Unimplemented

  (*TODO: Avi, code this*)
  let find k d =
    raise Unimplemented

  (*TODO: Avi, code this*)
  let member k d =
    raise Unimplemented

  (*TODO: Avi, code this*)
  let choose d =
    raise Unimplemented

  (*TODO: Uri, code this*)
  let to_list d =
    raise Unimplemented

  let expose_tree d =
    d

  (*TODO: Uri, code this*)
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