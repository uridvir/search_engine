open OUnit2
open Data
open Printf

exception Fine

module type Tests = sig
  val tests : OUnit2.test list
end

module IntKey = struct
	type t = int
	let compare a b =
		if a < b then `LT
		else if a = b then `EQ
		else `GT
	let format fmt i =
		Format.fprintf fmt "%d" i
end

module StringValue = struct
	type t = string
	let format fmt s =
		Format.fprintf fmt "\"%s\"" s
end

let foo = 0
let bar = "bar"
let not_foo = 1
let not_bar = "BAR"

let verbose = true

module RandomRepeat (D: Dictionary with type Key.t = int and type Value.t = string) = struct
  let entries = 75

  let folder f msg init _ =
    let random = (Random.int 19) + 1 in
    if verbose then msg random;
    let next = f random init in
    if verbose then Format.printf "\n%a\n" D.format next;
    next

  let test f init msg =
    let lots_of_nothing = List.init entries (fun _ -> ()) in
    Random.self_init ();
    try
      let result = List.fold_left (folder f msg) init lots_of_nothing in
      if verbose then printf "\nTest successful! Repeated for %d entries!\n" entries;
      assert D.(result |> rep_ok = result)
    with
    | D.DictionaryException d as e -> Format.printf "%a" D.format d; raise e
    | e -> raise e
end

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M: DictionaryMaker) = struct
	module D = M(IntKey)(StringValue)
  module R = RandomRepeat(D)

	let empty_test _ =
		assert D.(empty |> to_list = []);
		assert D.(is_empty empty)

  let insert_test _ =
    R.test (fun a b -> D.insert a "" b) D.empty (fun a -> printf "Inserting %d into dictionary..." a)

	let remove_test _ =
		R.test (fun a b -> D.remove a b) D.empty (fun a -> printf "Removing %d from dictionary..." a)

	let size_test _ =
    let f a b =
      let c = D.(insert a "" b) in
      if D.member a b then
        assert D.(size c = size b)
      else
        assert D.(size c = size b + 1);
      c
    in
		R.test f D.empty (fun a -> printf "Checking size after inserting %d" a)

	let member_test _ =
		assert D.(empty |> member foo = false);
		assert D.(empty |> insert foo bar |> member foo);
		assert D.(empty |> insert foo bar |> member not_foo = false);
		assert D.(empty |> insert foo bar |> remove foo |> member foo = false)

	let find_test _ =
		assert D.(empty |> insert foo bar |> find foo = Some bar);
		assert D.(empty |> insert foo bar |> find not_foo = None);
		assert D.(empty |> insert foo bar |> insert foo not_bar |> find foo = Some not_bar)

	let choose_test _ =
		assert D.(empty |> insert foo bar |> choose = Some (foo, bar));
		assert (
			match D.(empty |> insert foo bar |> insert not_foo not_bar |> choose) with
      (* Good, originally implemented to return first added, if it actually returns Some (not_foo, not_bar), the
      implementation isn't technically wrong *)
			| Some (k, v) -> true
      (* We're screwed *)
			| None -> false
		)

	let fold_test _ =
		let dicts =
		[
			D.(empty |> insert foo bar |> insert not_foo not_bar); (* 6 characters, key sum 1 *)
			D.(empty |> insert foo not_bar |> insert not_foo bar); (* 6 characters, key sum 1 *)
			D.(empty |> insert foo bar); (* 3 characters, key sum 0 *)
		]
		and funcs =
		[
			(fun k v init -> k + String.length v + init); (* Adds both keys and values *)
			(fun k v init -> String.length v + init); (* Adds just values *)
			(fun k v init -> k + init); (* Adds just keys *)
		]
		and results = [ [7; 6; 1]; [7; 6; 1]; [3; 3; 0] ] in
		(* What the HELL is this? A FOR loop? By God, why? *)
		for i = 0 to 2 do
			for j = 0 to 2 do
				if not D.(List.nth dicts i |> fold (List.nth funcs j) 0 = List.nth (List.nth results i) j) then
          failwith (sprintf "failed on dictionary %d, folder %d" i j)
			done
		done

	(*TODO: implement general test*)

	let tests =
		[
			"empty" 	>:: empty_test;
			"insert" 	>:: insert_test;
			"remove" 	>:: remove_test;
			"size" 		>:: size_test;
			"member" 	>:: member_test;
			"find" 		>:: find_test;
			"choose" 	>:: choose_test;
			"fold"		>:: fold_test;
		]
end

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)

module ListDictionaryTester = DictTester(MakeListDictionary)
module TreeDictionaryTester = DictTester(MakeTreeDictionary)

module MoreTreeTests = struct
	module D = MakeTreeDictionary(IntKey)(StringValue)
  module R = RandomRepeat(D)

	let type_test _ =
		assert
		(
			try
				ignore D.(empty |> expose_tree);
				raise Fine
			with
			| Fine -> true
			| _ -> false
		)

	let rep_ok_test _ =
		let badtrees =
		[
			Threenode
			{
				left3 = Twonode
				{
					left2 = Twonode {left2 = Leaf; value = (1, ""); right2 = Leaf};
					value = (2, "");
					right2 = Twonode {left2 = Leaf; value = (3, ""); right2 = Leaf};
				};
				lvalue = (4, "");
				middle3 = Twonode {left2 = Leaf; value = (5, ""); right2 = Leaf};
				rvalue = (6, "");
				right3 = Twonode {left2 = Leaf; value = (7, ""); right2 = Leaf};
			}
		]
		in
		if verbose then printf "\nAll these trees are invalid, and rep_ok should know that:\n";
		List.iter
		(
		fun t ->
			try

				if verbose then Format.printf "%a" D.format t;
				ignore D.(t |> rep_ok);
				raise Fine (*this should NOT be thrown, all the trees in this test are invalid*)
			with
			| Fine -> failwith "rep_ok should have failed!"
			| Failure _ -> ()
			| _ -> failwith "rep_ok produced unexpected behavior!"
		)
		(List.map (fun t -> D.import_tree t) badtrees);
		if verbose then printf "\nrep_ok test successful!\n"

  let literal_tree d =
    let open Format in
    let rec loop indent = function
      | Leaf -> fprintf str_formatter "Leaf"
      | Twonode {left2 = left; value = (k, v); right2 = right} ->
          fprintf str_formatter "Twonode\n";
          fprintf str_formatter "%s{\n" indent;
          fprintf str_formatter "%s\tleft2 = " indent;
            loop (indent ^ "\t") left;
          fprintf str_formatter ";\n";
          fprintf str_formatter "%s\tvalue = (%a, %a);\n" indent D.Key.format k D.Value.format v;
          fprintf str_formatter "%s\tright2 = " indent;
            loop (indent ^ "\t") right;
          fprintf str_formatter "\n";
          fprintf str_formatter "%s}" indent
      | Threenode {left3 = left; lvalue = (k1, v1); middle3 = middle; rvalue = (k2, v2); right3 = right} ->
          fprintf str_formatter "Threenode\n";
          fprintf str_formatter "%s{\n" indent;
          fprintf str_formatter "%s\tleft3 = " indent;
            loop (indent ^ "\t") left;
          fprintf str_formatter ";\n";
          fprintf str_formatter "%s\tlvalue = (%a, %a);\n" indent D.Key.format k1 D.Value.format v1;
          fprintf str_formatter "%s\tmiddle3 = " indent;
            loop (indent ^ "\t") middle;
          fprintf str_formatter ";\n";
          fprintf str_formatter "%s\trvalue = (%a, %a);\n" indent D.Key.format k2 D.Value.format v2;
          fprintf str_formatter "%s\tright3 = " indent;
            loop (indent ^ "\t") right;
          fprintf str_formatter "\n";
          fprintf str_formatter "%s}" indent
    in loop "" d; flush_str_formatter ()

  let n_tree_print n =
    let d = List.init n (fun a -> a) in
    let t = D.(List.fold_left
      (fun init a -> printf "\nInserting %d...\n" a; let d = insert a "" init in Format.printf "\n%a\n" D.format d; d)
      empty d |> expose_tree) in
    printf "\n%s\n" (literal_tree t)

  let hundred_tree = Test_trees.hundred_tree
  let twenty_tree = Test_trees.twenty_tree

  let remove_test _ =
    if verbose then begin
      printf "\nHundred tree:\n";
      Format.printf "%a\n" D.format D.(import_tree hundred_tree);
    end;
    R.test (fun a b -> D.remove a b) D.(import_tree hundred_tree) (fun a -> printf "\nRemoving %d from dictionary...\n" a)

	let tests =
		[
			"type"		>:: type_test;
			"rep_ok"	>:: rep_ok_test;
      "remove"  >:: remove_test;
		]

end

module MoreListTests = struct
	module D = MakeListDictionary(IntKey)(StringValue)

	let type_test _ =
		assert
		(
			try
				ignore D.(empty |> expose_tree);
				raise Fine
			with
			| Failure _ -> true
			| _ -> false
		)

	let tests =
		[
			"type"	>:: type_test;
		]
end

let tests = ListDictionaryTester.tests @ TreeDictionaryTester.tests @ MoreTreeTests.tests @ MoreListTests.tests
