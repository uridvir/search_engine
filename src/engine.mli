(* An [Engine] indexes words found in text files and answers
 * queries about which files contain which words. *)
module type Engine = sig

  (* The type of an index *)
  type idx

  (* [index_of_dir d] is an index of the files in [d].  Only files whose
   * names end in [.txt] are indexed.  Only [d] itself, not any
   * of its subdirectories, is indexed.
   * raises: Not_found if [d] is not a valid directory. *)
  val index_of_dir : string -> idx

  (* [to_list idx] is a list representation of [idx] as an association
   * list.  The first element of each pair in the list is a word,
   * the second element is a list of the files in which that word
   * appears.  The order of elements in both the inner and outer
   * lists is unspecified.  Likewise, it is unspecified whether
   * the outer list contains multiple entries for words that
   * are the same other than case, or just a single entry. *)
  val to_list : idx -> (string * string list) list

  (* [or_not idx ors nots] is a list of the files that contain
   * any of the words in [ors] and none of the words in [nots].
   * requires: [ors] is not empty. *)
  val or_not  : string list -> string list -> idx -> string list

  (* [and_not idx ands nots] is a list of the files that contain
   * all of the words in [ands] and none of the words in [nots].
   * requires: [ands] is not empty. *)
  val and_not : string list -> string list -> idx -> string list

  (* [format] is a printing function suitable for use
   * with the toplevel's [#install_printer] directive.
   * It outputs a textual representation of an index
   * on the given formatter. *)
  val format : Format.formatter -> idx -> unit

end

(* An engine implemented with list-based data structures.
 * Its performance is likely to be slow. *)
module ListEngine : Engine

(* An engine implemented with balanced-tree-based data structures.
 * Its performance is asymptotically more efficient
 * than [ListEngine]. *)
module TreeEngine : Engine
