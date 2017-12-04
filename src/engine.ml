open Data
open Printf

exception Unimplemented

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : string list -> string list -> idx -> string list
  val and_not : string list -> string list -> idx -> string list
  val format : Format.formatter -> idx -> unit
end

(*MakeEngine takes a Set to use as the type of the file list, and a Dictionary with string keys and Set values*)
module MakeEngine
  (S:Data.Set with type Elt.t = string)
  (D:Data.Dictionary with type Key.t = string and type Value.t = S.t)
  : Engine
=
struct
  type idx = D.t

  let index_of_dir dir =
    Printf.printf "dir = %s" dir;
    let files = Sys.readdir dir |> Array.to_list in

    let process_file init file =
      let filename = dir ^ "/" ^ file in
      let _in = open_in filename in
      let n = in_channel_length _in in
      let text = Bytes.create n in
      let _ = input _in text 0 n in
      let text = Bytes.unsafe_to_string text in
      close_in _in;

      let format = ".,\"':;!" in
      let text = String.map (fun (c: char) -> if String.contains format c then ' ' else c) text in
      let words = List.filter (fun str -> not (str = "")) (String.split_on_char ' ' text) in

      let add_to_index index word =
        match D.(index |> find word) with
        | Some files ->
          if not S.(files |> member file) then
            D.(index |> insert word S.(files |> insert file))
          else
            index
        | None -> D.(index |> insert word S.(empty |> insert file))
      in
      List.fold_left add_to_index init words
    in
    List.fold_left process_file D.empty files

  let to_list index =
    let (words, file_sets) = List.split D.(index |> to_list) in
    List.combine words (List.map (fun a -> S.(a |> to_list)) file_sets)

  let or_not ors nots index =
    (*get all files with or words*)
    let has_ors =
      S.(List.fold_left (fun init a -> union init (match D.(index |> find a) with | Some files -> files | None -> empty)) empty ors)
    in

    (*filters out files with not words*)
    let also_lacks_nots = 
      S.(List.fold_left (fun init a -> difference init (match D.(index |> find a) with | Some files -> files | None -> empty)) has_ors nots)
    in 
    S.(also_lacks_nots |> to_list) 

  let and_not ands nots index =
    (*get all files*)
    let files =
      S.(List.fold_left (fun init a -> union init (match D.(index |> find a) with | Some files -> files | None -> empty)) empty (ands @ nots))
    in

    (*gets all files with all and words*)
    let has_all_ands =
      S.(List.fold_left (fun init a -> intersect init (match D.(index |> find a) with | Some files -> files | None -> empty)) files ands)
    in

    let also_lacks_nots = 
      S.(List.fold_left (fun init a -> difference init (match D.(index |> find a) with | Some files -> files | None -> empty)) has_all_ands nots)
    in
    S.(also_lacks_nots |> to_list) 

  let format fmt idx =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

module TrivialEngine =
struct
  type idx = unit
  let index_of_dir d = ()
  let to_list idx = []
  let or_not ors nots idx = []
  let and_not ands nots idx = []
  let format fmt idx = ()
end

module StringKey = struct
  type t = string
  let compare a b =
    if String.compare a b < 0 then `LT
    else if String.compare a b = 0 then `EQ
    else `GT 
  let format fmt s =
    Format.fprintf fmt "%s" s
end

module ListSet = MakeSetOfDictionary(StringKey)(MakeListDictionary)
module TreeSet = MakeSetOfDictionary(StringKey)(MakeTreeDictionary)

module ListEngine = MakeEngine(ListSet)(MakeListDictionary(StringKey)(ListSet))
module TreeEngine = MakeEngine(TreeSet)(MakeTreeDictionary(StringKey)(TreeSet))