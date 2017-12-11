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
  (*Extension to spec:*)
  val import_list : (key * value) list -> t
  val expose_tree : t -> (key, value) tree23
  (*Extension to spec:*)
  val import_tree : (key, value) tree23 -> t
  exception TreeException of t
  exception ListException of t
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

  exception TreeException of t
  exception ListException of t

  (*Makes sure this dictionary is valid. Is currently IMPROPERLY IMPLEMENTED and performs no checks.*)
  let rec rep_ok = function
    | (h :: t) as d ->
        let (k, _) = h in
        if List.exists (fun a -> let (k1, _) = a in (k = k1)) t then
          failwith "Bad list!"
        else if rep_ok t = t then
          d
        else
          failwith "Bad list!"
    | [] -> []

  (*A variable representing an empty list of type t *)
  let empty = []

  (*Checks if a dictionary d is empty by comparing to the empty variable.*)
  let is_empty d = let d = rep_ok d in (d = empty)

  (*Runs rep_ok and then returns the number of key-value pairs in the dictionary. It does this by getting the length of the list of tuples.*)
  let size d =
    let d = rep_ok d in 
    List.length d

  (*
  Removes all values bound to key k from dictionary. The anonymous function checks if the key of 'a' (k1) does not equal k. The filter function returns a new dictionary of only the
  keys that do NOT equal k (and their associated values).
  *)
  let remove k d =
    let d = rep_ok d in
    List.filter (fun a -> let k1, _ = a in Key.compare k k1 != `EQ) d 

  (*
  Inserts value v with key k into dictionary d and returns resulting dictionary. First, uses remove function to remove all previous keys bound to k Finally, appends tuple of the key
  and value inputted to the dictionary.
  *)
  let insert k v d =
    let d = d |> rep_ok |> remove k in
    d @ [(k, v)]

  (*
  Finds if key k is bound in dictionary. The anonymous function checks if the key of 'a' (k1) equals k. The exists function checks if any pair satisfies the condition.
  *)
  let member k d =
    let d = rep_ok d in
    List.exists (fun a -> let k1, _ = a in Key.compare k k1 = `EQ) d

  (*
  Finds the value bound to the key k in dictionary. First it checks that the key is in the dictionary. If not, it returns None. Otherwise: The anonymous function here checks if the key
  of the tuple equals k.
  *)
  let find k d =
    let d = rep_ok d in 
    if member k d then
      let _, v = List.find (fun a -> let k1, _ = a in Key.compare k k1 = `EQ) d in 
      Some v
    else
      None

  (* Takes in a dictionary, returns the first element if it isn't empty *)
  let choose d =
    let d = rep_ok d in
    if is_empty d then None else Some (List.hd d)

  (*
  Since d is already an association list, it just sorts it. The anonymous function sorts the keys, and the sort function utilizes the anonymous function to sort the association list.
  *)
  let to_list d =
    let d = rep_ok d in
    List.sort (fun a b -> let k, _ = a and k1, _ = b in match Key.compare k k1 with | `LT -> -1 | `EQ -> 0 | `GT -> 1) d

  let import_list l = l

  let fold f init d =
    let d = rep_ok d in
    List.fold_left (fun init a -> let (key, value) = a in f key value init) init d

  let expose_tree d =
    failwith "not a 2-3 tree"

  let import_tree tree =
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

  exception TreeException of t
  exception ListException of t

  let rec all_lengths = function
    | Twonode {left2 = left; value = _; right2 = right} ->
        List.map (fun a -> a + 1) (all_lengths left @ all_lengths right)
    | Threenode {left3 = left; lvalue = _; middle3 = middle; rvalue = _; right3 = right} ->
        List.map (fun a -> a + 1) (all_lengths left @ all_lengths middle @ all_lengths right)
    | Leaf -> [0]
    
  let branch t cmp k = 
    match t with
    | Leaf -> true
    | Twonode {left2 = _; value = (k1, v1); right2 = _} when Key.compare k1 k = cmp -> true
    | Threenode {left3 = _; lvalue = (k1, v1); middle3 = _; rvalue = (k2, v2); right3 = _} when Key.compare k1 k = cmp && Key.compare k2 k = cmp -> true
    | _ -> false

  let rec order_check = function
    | Leaf -> true
    | Twonode {left2 = left; value = (k, v); right2 = right} when branch left `LT k && branch right `GT k &&
      order_check left && order_check right -> true
    | Threenode {left3 = left; lvalue = (k, v); middle3 = middle; rvalue = (k1, v1); right3 = right} when Key.compare k k1 = `LT &&
      branch left `LT k && branch middle `GT k && branch middle `LT k1 && branch right `GT k1 && order_check left && order_check middle &&
      order_check right -> true
    | _ -> false

  let rep_ok d =
    let lengths = all_lengths d in
    let first = List.hd lengths in
    let rest = List.tl lengths in
    if List.for_all (fun a -> (a = first)) rest then
      if order_check d then
        d
      else
        failwith "Bad tree! Failed order_check"
    else
      failwith "Bad tree! Failed all_lengths check"
 
  let empty = Leaf

  let is_empty d = let d = rep_ok d in (d = Leaf)

  let rec size d =
    let d = rep_ok d in 
    match d with
    | Leaf -> 0
    | Twonode {left2 = left; value = _; right2 = right} -> 1 + size left + size right
    | Threenode {left3 = left; lvalue = _; middle3 = middle; rvalue = _; right3 = right} -> 2 + size left + size middle + size right

  let rec to_list d =
    let d = rep_ok d in
    match d with
    | Leaf -> []
    | Twonode {left2 = left; value = (k1, v1); right2 = right} ->
        to_list left @ [(k1, v1)] @ to_list right
    | Threenode {left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} ->
        to_list left @ [(k1, v1)] @ to_list middle @ [(k2, v2)] @ to_list right

  let import_list l =
    failwith "Not a list"

  let single_length d =
    let lengths = all_lengths d in
    let first = List.hd lengths in
    let rest = List.tl lengths in
    if List.for_all (fun a -> (a = first)) rest then
      first
    else
      raise (TreeException d)

  let insert_up d =
    match d with
    (*tree is balanced*)
    | Twonode {left2 = left; value = _; right2 = right} as node when single_length left = single_length right -> 
        node
    | Threenode {left3 = left; lvalue = _; middle3 = middle; rvalue = _; right3 = right} as node when single_length left = single_length middle && single_length middle = single_length right -> 
        node
    
    (*
     *        x
     *      /   \
     *     w     r  -->     w x
     *   /   \            /  |  \
     * l      m          l   m   r
     *)
    | Twonode {left2 = Twonode {left2 = l; value = w; right2 = m} as left; value = x; right2 = r} when single_length left = single_length r + 1 ->
        Threenode {left3 = l; lvalue = w; middle3 = m; rvalue = x; right3 = r}

    (*
     *     x
     *   /   \
     * l      w     -->     x w  
     *      /   \         /  |  \
     *     m     r       l   m   r
     *)
    | Twonode {left2 = l; value = x; right2 = Twonode {left2 = m; value = w; right2 = r} as right} when single_length l + 1 = single_length right ->
        Threenode {left3 = l; lvalue = x; middle3 = m; rvalue = w; right3 = r}

    (*
     *       x y                 x
     *     /  |  \             /   \
     *    w   c   d  -->     w       y
     *  /   \              /   \   /   \
     * a     b            a    b   c    d
     *)
    | Threenode {left3 = Twonode {left2 = a; value = w; right2 = b} as left; lvalue = x; middle3 = c; rvalue = y; right3 = d} when single_length left = single_length c + 1 && single_length c = single_length d ->
        Twonode {left2 = Twonode {left2 = a; value = w; right2 = b}; value = x; right2 = Twonode {left2 = c; value = y; right2 = d}}
    
    (*
     *    x y                 w
     *  /  |  \             /   \
     * a   w   d  -->     x       y
     *   /   \          /   \   /   \
     *  b     c        a    b   c    d
     *)
    | Threenode {left3 = a; lvalue = x; middle3 = Twonode {left2 = b; value = w; right2 = c} as middle; rvalue = y; right3 = d} when single_length a + 1 = single_length middle && single_length middle = single_length d + 1 ->
        Twonode {left2 = Twonode {left2 = a; value = x; right2 = b}; value = w; right2 = Twonode {left2 = c; value = y; right2 = d}}
    
    (*
     *    x y                   y
     *  /  |  \               /   \
     * a   b   w    -->     x      w
     *       /   \        /   \  /   \
     *       c   d       a    b  c    d
     *)
    | Threenode {left3 = a; lvalue = x; middle3 = b; rvalue = y; right3 = Twonode {left2 = c; value = w; right2 = d} as right} when single_length a = single_length b && single_length b + 1 = single_length right ->
        Twonode {left2 = Twonode {left2 = a; value = x; right2 = b}; value = y; right2 = Twonode {left2 = c; value = w; right2 = d}}
    | d -> Printf.printf "\nFailed to find a case, bad tree:\n"; raise (TreeException d)

  (*takes three key-value tuples and sorts them by key, returning a tuple of tuples*)
  let sort_three a b c =
    let sorter a b =
      let k, _ = a and k1, _ = b in
      match Key.compare k k1 with
      | `LT -> -1 
      | `EQ -> 0 
      | `GT -> 1
    in
    match List.sort sorter [a; b; c] with
    | a :: b :: c :: [] -> (a, b, c)
    | _ -> failwith "Couldn't deconstruct list!"
  
  let rec insert_down k v = function
    (*terminal two node to fill*)
    | Twonode {left2 = Leaf; value = (k1, v1); right2 = Leaf} ->
        if Key.compare k k1 = `LT then 
          Threenode {left3 = Leaf; lvalue = (k, v); middle3 = Leaf; rvalue = (k1, v1); right3 = Leaf}
        else 
          Threenode {left3 = Leaf; lvalue = (k1, v1); middle3 = Leaf; rvalue = (k, v); right3 = Leaf}
    (*terminal three node to fill*)
    | Threenode {left3 = Leaf; lvalue = (k1, v1); middle3 = Leaf; rvalue = (k2, v2); right3 = Leaf} ->
        let (first, second, last) = sort_three (k1, v1) (k2, v2) (k, v) in
        Twonode {
          left2 = Twonode {left2 = Leaf; value = first; right2 = Leaf};
          value = second;
          right2 = Twonode {left2 = Leaf; value = last; right2 = Leaf}
        }
    (*empty tree to fill*)
    | Leaf -> Twonode {left2 = Leaf; value = (k, v); right2 = Leaf}
    (*two node with child to explore*)
    | Twonode({left2 = left; value = (k1, v1); right2 = right} as node) ->
        let result =
          if Key.compare k k1 = `LT then
            (*explore left*)
            Twonode {node with left2 = insert_down k v left}
          else
            (*explore right*)
            Twonode {node with right2 = insert_down k v right}
        (*kick up the result to the next layer*)
        in insert_up result
    (*three node with child to explore*)
    | Threenode({left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} as node) ->
        let result =
          if Key.compare k k1 = `LT then
            (*explore left*)
            Threenode {node with left3 = insert_down k v left}
          else if Key.compare k k2 = `LT then
            (*explore middle*)
            Threenode {node with middle3 = insert_down k v middle}
          else
            (*exlore right*)
            Threenode {node with right3 = insert_down k v right}
        (*kick up the result to the next layer*)
        in insert_up result
    (*"I want to explore your child." Sounds like something a pedophile would say...*)

  let rec remove_successor = function
    | Twonode {left2 = Twonode {left2 = Leaf; value = v; right2 = Leaf}; value = w; right2 = Twonode {left2 = Leaf; value = x; right2 = Leaf}} -> 
        (v, insert_up (Threenode {left3 = Leaf; lvalue = w; middle3 = Leaf; rvalue = x; right3 = Leaf}))
    | Threenode {left3 = Leaf; lvalue = v; middle3 = Leaf; rvalue = w; right3 = Leaf} ->
        (v, insert_up (Twonode {left2 = Leaf; value = w; right2 = Leaf}))
    | Twonode {left2 = left; value = _; right2 = _} -> 
        let (successor, branch) = remove_successor left in (successor, insert_up branch)
    | Threenode {left3 = left; lvalue = _; middle3 = _; rvalue = _; right3 = _} -> 
        let (successor, branch) = remove_successor left in (successor, insert_up branch)
    | d -> raise (TreeException d)

  let rec remove_initial k = function
    | Leaf -> Leaf
    | Twonode ({left2 = left; value = (k1, v1); right2 = right} as node) ->
        if Key.compare k k1 = `LT then 
          remove_initial k left |> insert_up
        else if Key.compare k k1 = `EQ then
          let (successor, branch) = remove_successor right in
          insert_up (Twonode {node with value = successor; right2 = branch})
        else 
          remove_initial k right |> insert_up
    | Threenode ({left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} as node) ->
        if Key.compare k k1 = `LT then
          remove_initial k left |> insert_up
        else if Key.compare k k1 = `EQ then 
          let (successor, branch) = remove_successor middle in
          insert_up (Threenode {node with lvalue = successor; middle3 = branch})
        else if Key.compare k k2 = `LT then 
          remove_initial k middle |> insert_up
        else if Key.compare k k2 = `EQ then
          let (successor, branch) = remove_successor right in
          insert_up (Threenode {node with rvalue = successor; right3 = branch})
        else 
          remove_initial k right |> insert_up

  let remove k d =
    let d = rep_ok d in
    d |> remove_initial k

  let insert k v d =
    let d = rep_ok d in
    d (*|> remove k*) |> insert_down k v

  let rec find k d =
    let d = rep_ok d in
    match d with
    | Leaf -> None
    | Twonode {left2 = left; value = (k1, v1); right2 = right} ->
        if Key.compare k k1 = `LT then find k left
        else if Key.compare k k1 = `EQ then Some v1
        else find k right
    | Threenode {left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} ->
        if Key.compare k k1 = `LT then find k left
        else if Key.compare k k1 = `EQ then Some v1
        else if Key.compare k k2 = `LT then find k middle
        else if Key.compare k k2 = `EQ then Some v2
        else find k right

  let member k d =
    let d = rep_ok d in
    match find k d with | Some v -> true | None -> false

  let choose d = 
    let d = rep_ok d in 
    match d with
    | Twonode {left2 = _; value = (k, v); right2 = _} -> Some (k, v)
    | Threenode {left3 = _; lvalue = (k, v); middle3 = _; rvalue = _; right3 = _} -> Some (k, v)
    | Leaf -> None 

  let fold f init d =
    let d = rep_ok d in
    List.fold_left (fun init a -> let (k, v) = a in f k v init) init (to_list d)

  let expose_tree d = d

  let import_tree t = rep_ok t

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

  let empty = D.empty

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
    D.fold (fun x _ init -> f x init) init s

  let to_list s =
    let lst = D.(s |> to_list) in
    let (xs, _) = List.split lst in
    xs

  let union s1 s2 =
    s1 |> fold (fun x init -> init |> insert x) s2

  let intersect s1 s2 =
    s1 |> fold (fun x init -> if s2 |> member x then init |> insert x else init) empty

  let difference s1 s2 =
    s1 |> fold (fun x init -> if not (s2 |> member x) then init |> insert x else init) empty

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end