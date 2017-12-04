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
    let rec _size = function
      | Leaf -> 0
      | Twonode {left2 = left; value = _; right2 = right} -> 1 + _size left + _size right
      | Threenode {left3 = left; lvalue = _; middle3 = middle; rvalue = _; right3 = right} -> 2 + _size left + _size middle + _size right
    in _size d

  (*
   * AVI PLEASE READ:
   * 
   * I found out how to pattern match these 2-3 trees, it's like this. For example, a match on a Twonode might look like:
   *
   *    match d with
   *    | Twonode {left2 = left; value = (k1, v1); right2 = right} when left != Leaf && right != Leaf -> *something*
   *
   * The idea is that a pattern match for a record is supposed to look like declaring a record. (Relevant Stack Overflow question: 
   * https://stackoverflow.com/questions/17173690/record-type-pattern-matching-in-ocaml) The when keyword is something I found out about
   * looking at the OCaml documentation (I won't bore you with that): you just throw it at the end of a pattern match case and the match will
   * only complete if the condition is true. Here, I check if both left and right aren't leaves.
   *
   * Hope it was helpful.
   *)

   (*TODO: Uri, code this*)
  let to_list d =
    let rec descend d = match d with
      | Leaf -> []
      | Twonode {left2 = left; value = (k1, v1); right2 = right} ->
          [(k1, v1)] @ (descend left) @ (descend right)
      | Threenode {left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} ->
          [(k1, v1); (k2, v2)] @ (descend left) @ (descend middle) @ (descend right)
    in

    let unsorted = descend d in
    List.sort (fun a b -> let k1, _ = a and k2, _ = b in match (Key.compare k1 k2) with | `LT -> -1 | `EQ -> failwith "No two keys should be equal" | `GT -> 1) unsorted

  (*TODO: Uri, code this*)
  let insert k v d =
    (*takes three key-value tuples and sorts them by key, returning a tuple of tuples*)
    let sort_three a b c =
      let (a, b, c) = match (List.sort (fun a b -> let k, _ = a and k1, _ = b in match Key.compare k k1 with | `LT -> -1 | `EQ -> 0 | `GT -> 1) [a; b; c]) with
        | a :: b :: c :: [] -> (a, b, c)
        | _ -> failwith "Couldn't deconstruct list!" 
      in (a, b, c)
    in
    let rec descend k v d =
      match d with
      (*three node to fill on the left (two node parent)*)
      | Twonode {left2 = Threenode {left3 = Leaf; lvalue = (k2, v2); middle3 = Leaf; rvalue = (k3, v3); right3 = Leaf}; 
        value = (k1, v1); right2 = right} when Key.compare k k1 = `LT ->
          let (first, second, last) = sort_three (k, v) (k2, v2) (k3, v3) in
          Threenode 
          {
            left3 = Twonode {left2 = Leaf; value = first; right2 = Leaf}; 
            lvalue = second;
            middle3 = Twonode {left2 = Leaf; value = last; right2 = Leaf}; 
            rvalue = (k1, v1); 
            right3 = right;
          }

      (*three node to fill on the right (two node parent)*)
      | Twonode {left2 = left; value = (k1, v1); right2 = Threenode {left3 = Leaf; lvalue = (k2, v2); middle3 = Leaf; rvalue = (k3, v3); right3 = Leaf}} 
        when Key.compare k k1 != `LT ->
          let (first, second, last) = sort_three (k, v) (k2, v2) (k3, v3) in
          Threenode {
            left3 = left;
            lvalue = (k1, v1);
            middle3 = Twonode {left2 = Leaf; value = first; right2 = Leaf};
            rvalue = second;
            right3 = Twonode {left2 = Leaf; value = last; right2 = Leaf};
          }

      (*three node to fill on the left (three node parent)*)
      | Threenode {left3 = Threenode {left3 = Leaf; lvalue = (k3, v3); middle3 = Leaf; rvalue = (k4, v4); right3 = Leaf};
        lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} when Key.compare k k1 = `LT ->
          let (first, second, last) = sort_three (k, v) (k3, v3) (k4, v4) in
          Twonode 
            {
              left2 = Twonode 
                { 
                  left2 = Twonode {left2 = Leaf; value = first; right2 = Leaf};
                  value = second;
                  right2 = Twonode {left2 = Leaf; value = last; right2 = Leaf}
                };
              value = (k1, v1);
              right2 = Twonode {left2 = middle; value = (k2, v2); right2 = right}
            }

      (*three node to fill in the middle (three node parent)*)
      | Threenode {left3 = left; lvalue = (k1, v1); middle3 = Threenode {left3 = Leaf; lvalue = (k3, v3); middle3 = Leaf; rvalue = (k4, v4); right3 = Leaf};
        rvalue = (k2, v2); right3 = right} when Key.compare k k1 != `LT && Key.compare k k2 = `LT ->
          let (first, second, last) = sort_three (k, v) (k3, v3) (k4, v4) in
          Twonode 
            {
              left2 = Twonode 
                { 
                  left2 = left;
                  value = (k1, v1);
                  right2 = Twonode {left2 = Leaf; value = first; right2 = Leaf}
                };
              value = second;
              right2 = Twonode 
              {
                left2 = Twonode {left2 = Leaf; value = last; right2 = Leaf};
                value = (k2, v2); 
                right2 = right
              }
            }
      
      (*three node to fill on the right (three node parent)*)
      | Threenode {left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); 
        right3 = Threenode {left3 = Leaf; lvalue = (k3, v3); middle3 = Leaf; rvalue = (k4, v4); right3 = Leaf}} when Key.compare k k1 != `LT && Key.compare k k2 != `LT ->
          let (first, second, last) = sort_three (k, v) (k3, v3) (k4, v4) in
          Twonode 
            {
              left2 = Twonode {left2 = left; value = (k1, v1); right2 = middle};
              value = (k2, v2);
              right2 = Twonode 
                {
                  left2 = Twonode {left2 = Leaf; value = first; right2 = Leaf};
                  value = second;
                  right2 = Twonode {left2 = Leaf; value = last; right2 = Leaf}
                }
            }

      (*two node to fill*)
      | Twonode {left2 = Leaf; value = (k1, v1); right2 = Leaf} ->
          if Key.compare k k1 = `LT then 
            Threenode {left3 = Leaf; lvalue = (k, v); middle3 = Leaf; rvalue = (k1, v1); right3 = Leaf}
          else 
            Threenode {left3 = Leaf; lvalue = (k1, v1); middle3 = Leaf; rvalue = (k, v); right3 = Leaf}
      
      (*two node with child to explore*)
      | Twonode({left2 = left; value = (k1, v1); right2 = right} as node) ->
          (*explore left*)
          if Key.compare k k1 = `LT then 
            Twonode {node with left2 = descend k v left}
          (*explore right*)
          else
            Twonode {node with right2 = descend k v right}
      
      (*three node with child to explore*)
      | Threenode({left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} as node) ->
          (*explore left*)
          if Key.compare k k1 = `LT then
            Threenode {node with left3 = descend k v left}
          (*explore middle*)
          else if Key.compare k k2 = `LT then
            Threenode {node with middle3 = descend k v middle}
          (*exlore right*)
          else if Key.compare k k2 != `LT then
            Threenode {node with right3 = descend k v right}
          else
            failwith "Key doesn't fit"

      | Leaf -> Twonode {left2 = Leaf; value = (k, v); right2 = Leaf}

      (*"I want to explore your child." Sounds like something a pedophile would say...*)
    in descend k v d
    (*TODO: Make insert function remove previous bindings of k*)

   (*TODO: Uri, code this*)
  let remove k d =
    d |> to_list |> List.filter (fun a -> let k1, _ = a in Key.compare k k1 != `EQ) |> 
    List.fold_left (fun init a -> let k1, v1 = a in init |> insert k1 v1) empty

  (*TODO: Avi, code this*)
  let find k d =
    let rec descend d = match d with
      | Leaf -> None
      | Twonode {left2 = left; value = (k1, v1); right2 = right} ->
          if Key.compare k k1 = `LT then descend left
          else if Key.compare k k1 = `EQ then Some v1
          else descend right
      | Threenode {left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} ->
          if Key.compare k k1 = `LT then descend left
          else if Key.compare k k1 = `EQ then Some v1
          else if Key.compare k k2 = `LT then descend middle
          else if Key.compare k k2 = `EQ then Some v2
          else descend right
    in descend d 

  (*TODO: Avi, code this*)
  let member k d =
    match (find k d) with | Some v -> true | None -> false

  (*TODO: Avi, code this*)
  let choose d =
    match d with
      | Twonode {left2 = _; value = (k, v); right2 = _} -> Some (k, v)
      | Threenode {left3 = _; lvalue = (k, v); middle3 = _; rvalue = _; right3 = _} -> Some (k, v)
      | Leaf -> None 

  let expose_tree d =
    d

  (*TODO: Uri, code this*)
  let fold f init d =
    List.fold_left (fun init a -> let (k, v) = a in f k v init) init (to_list d)

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

module NullValue = struct
  type t = unit
  let format fmt s = failwith "Trying to format null value"
end

module MakeSetOfDictionary (C : Comparable) (DM:DictionaryMaker) = struct
  module Elt = C
  type elt = Elt.t

  (* TODO: change type [t] to something involving a dictionary *)
  module D = DM(C)(NullValue)
  type t = D.t

  let rep_ok s =
    raise Unimplemented

  let empty =
    D.empty

  let is_empty s =
    D.(s |> is_empty)

  let size s =
    D.(s |> size)

  let insert x s =
    D.(s |> insert x ())

  let member x s =
    D.(s |> member x)

  let remove x s =
    D.(s |> remove x)

  let choose s =
    match D.(s |> choose) with
    | Some (x, _) -> Some x
    | None -> None

  let fold f init s =
    List.fold_left (fun init a -> let x, _ = a in f x init) init D.(s |> to_list)

  let to_list s =
    let lst = D.(s |> to_list) in
    let (xs, _) = List.split lst in
    xs

  let union s1 s2 =
    D.(s1 |> to_list |> List.fold_left (fun init a -> let x, _ = a in init |> insert x ()) s2)

  let intersect s1 s2 =
    D.(s1 |> to_list |> List.fold_left (fun init a -> let x, _ = a in if s2 |> member x then init |> insert x () else init) empty)

  let difference s1 s2 =
    D.(s1 |> to_list |> List.fold_left (fun init a -> let x, _ = a in if not (s2 |> member x) then init |> insert x () else init) empty)

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end