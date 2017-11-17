open OUnit2
open Data
open Printf

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
		Format.fprintf fmt "%s" s
end

let foo = 0
let bar = "bar"
let not_foo = 1
let not_bar = "BAR"

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M: DictionaryMaker) = struct
	module D = M(IntKey)(StringValue)

	let empty_test _ =
		assert D.(empty |> to_list = []);
		assert D.(is_empty empty)

	let insert_test _ =
		assert D.(empty |> insert foo bar |> to_list = [(foo, bar)]);
		assert D.(empty |> insert foo bar |> insert not_foo not_bar |> to_list = [(foo, bar); (not_foo, not_bar)]);
		assert D.(empty |> insert foo bar |> insert foo not_bar |> to_list = [(foo, not_bar)])

	let remove_test _ =
		assert D.(empty |> insert foo bar |> remove foo |> to_list = []);
		assert D.(empty |> insert foo bar |> remove not_foo |> to_list = [(foo, bar)]);
		assert D.(empty |> insert foo bar |> insert not_foo not_bar |> remove not_foo |> to_list = [(foo, bar)])

	let size_test _ =
		assert D.(empty |> size = 0);
		assert D.(empty |> insert foo bar |> size = 1);
		assert D.(empty |> insert foo bar |> insert foo not_bar |> size = 1);
		assert D.(empty |> insert foo bar |> insert not_foo not_bar |> size = 2)

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
			| Some (k, v) -> true (*Good, originally implemented to return first added, if it actually returns Some (not_foo, not_bar), the implementation isn't technically wrong*)
			| None -> false (*We're screwed*)
		)

	let fold_test _ =
		let dicts = 
		[
			D.(empty |> insert foo bar |> insert not_foo not_bar); (*6 characters, key sum 1*)
			D.(empty |> insert foo not_bar |> insert not_foo bar); (*6 characters, key sum 1*)
			D.(empty |> insert foo bar); (*3 characters, key sum 0*)
		]
		and funcs =
		[
			(fun k v init -> k + String.length v + init); (*Adds both keys and values*)
			(fun k v init -> String.length v + init); (*Adds just values*)
			(fun k v init -> k + init); (*Adds just keys*)
		]
		and results = [ [7; 6; 1]; [7; 6; 1]; [3; 3; 0] ] in
		(*What the HELL is this? A FOR loop? By God, why?*)
		for i = 0 to 2 do
			for j = 0 to 2 do
				if not D.(List.nth dicts i |> fold (List.nth funcs j) 0 = List.nth (List.nth results i) j) then failwith (sprintf "failed on dictionary %d, folder %d" i j)
			done
		done

	(*TODO: implement general test*)
	let general_test _ = assert true

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

let tests = ListDictionaryTester.tests

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)