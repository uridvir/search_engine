open Data
open Printf

exception Unimplemented

module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module MakeEngine
  (S:Data.Set with type Elt.t = string)
  (D:Data.Dictionary with type Key.t = string)
  : Engine
=
struct
  type idx = unit

  let index_of_dir d =
    raise Unimplemented

  let to_list idx =
    raise Unimplemented

  let or_not idx ors nots =
    raise Unimplemented

  let and_not idx ands nots =
    raise Unimplemented

  let format fmt idx =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end

module TrivialEngine =
struct
  type idx = unit
  let index_of_dir d = ()
  let to_list idx = []
  let or_not idx ors nots = []
  let and_not idx ands nots = []
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

module StringListValue = struct
  type t = string list
  let format fmt slst =
    List.iter (Format.fprintf fmt "%s ") slst
end

module D = MakeListDictionary(StringKey)(StringListValue)

module ListEngine = struct

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
          if List.exists (fun a -> not (a = file)) files then
            D.(index |> insert word (files @ [file]))
          else
            index
        | None -> d.(index |> insert word [file])
      in

      List.fold_left add_to_index init words
    in

    List.fold_left process_file D.empty files

  let to_list index = D.(index |> to_list)

  let or_not index ors nots =
    (*
    if (List.exists (List.exists (fun a -> let word, _ = a in List.exists (fun b -> word = b) ors) index) then
      if (* Index does not contain Nots *) then
        (* List of new Index *)
      else
      []
    else
      []
    *)
    raise Unimplemented

  (*
   * and_not
   * 
   * Arguments:
   * 
   * Returns: All files in index that have all words in ands and none of the
   * words in nots.
   * 
   * Algorithm:
   * 
   * 1) Get a list of all the files
   *    1a) Create a list of files, starting off empty
   *    1b) For all words in index
   *        1ba) For all files the word appears in
   *              1baa) Is that file on the list of files (step 1a)?
   *                    1baaa) Yes: no action
   *                    1baab) No: add file to list of files (step 1aa)
   * 
   * 2) For each word in ands
   *    2a) Is word in index?
   *        2aa) Yes: proceed
   *        2ab) No: whole function returns empty list
   *    2b) For each file in list (step 1)
   *        2ba) Is that file on the list of files where word was found?
   *              2baa) Yes: Keep file
   *              2bab) No: Remove file
   *
   * 3) For each word in nots
   *    2a) Is word in index?
   *        2aa) Yes: Proceed
   *        2ab) No: skip this one
   *    3a) For each file in list (step 1)
   *        3aa) Is that file on the list of files where word was found?
   *              3aaa) Yes: Remove file
   *              3aab) No: Keep file
   *
   * 4) Return list of files
   *)
  let and_not index ands nots =
      (*check if lst contains item a*)
      let contains a lst = List.exists (fun b -> a = b) lst in
      
      (*index as association list*)
      let index_list = D.(index |> to_list) in

      (*find all the files in the index*)
      let files =
        let f : (string list -> string -> string list) = (fun init a -> if init |> contains a then init else init @ [a])
        and g : (string list -> string * string list -> string list) = (fun init a -> let _, files = a in List.fold_left f init files) in
        List.fold_left g [] index_list
      in

      (*filter files out that don't have all and words*)
      let files =
        let f : (string -> string -> bool) = (fun a b -> match D.(index |> find b) with | Some files -> files |> contains a | None -> false) in
        List.filter (fun a -> List.for_all (f a) ands) files
      in

      (*filters files out that have any not words*)
      let f : (string -> string -> bool) = (fun a b -> match D.(index |> find b) with | Some files -> not (files |> contains a) | None -> true) in
      List.filter (fun a -> if List.for_all (f a) nots) files

  let format fmt index =
    raise Unimplemented

end
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)

module TreeEngine = TrivialEngine
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)
