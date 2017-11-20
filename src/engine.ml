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

module IndexListDictionary = MakeListDictionary(StringKey)(StringListValue)

module ListEngine = struct

  type idx = IndexListDictionary.t

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
        match (index |> IndexListDictionary.find word) with
        | Some files ->
          if List.exists (fun a -> not (a = file)) files then
            index |> IndexListDictionary.insert word (files @ [file])
          else
            index
        | None -> index |> IndexListDictionary.insert word [file]
      in

      List.fold_left add_to_index init words
    in

    List.fold_left process_file IndexListDictionary.empty files

  let to_list index = index |> IndexListDictionary.to_list

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

  let and_not index ands nots =
    raise Unimplemented

  let format fmt index =
    raise Unimplemented

end
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)

module TreeEngine = TrivialEngine
(* TODO: replace [TrivialEngine] in the line above with
   an application of [MakeEngine] to some appropriate parameters. *)
