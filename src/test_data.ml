open OUnit2
open Data

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

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M: DictionaryMaker) = struct
	module TestDictionary = M(IntKey)(StringValue)
	let foo = 0
	let bar = "bar"
	let not_foo = 1
	let not_bar = "BAR"
	let tests =
		[
		"empty"		>:: (fun _ -> assert TestDictionary.(empty |> to_list = []));
	  	"is_empty" 	>:: (fun _ -> assert TestDictionary.(is_empty empty));
	  	"insert"	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> to_list = [(foo, bar)]));
	  	"insert2"	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> insert foo not_bar |> to_list = [(foo, not_bar)]));
	  	"size" 		>:: (fun _ -> assert TestDictionary.(size empty = 0));
	  	"size2" 	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> size = 1));
	  	"member" 	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> member foo));
	  	"member2"	>:: (fun _ -> assert TestDictionary.(empty |> insert not_foo not_bar |> member foo = false));
	  	"find" 		>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> find foo = Some bar));
	  	"find2" 	>:: (fun _ -> assert TestDictionary.(empty |> find not_foo = None));
	  	"remove" 	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> remove foo = empty));
	  	"remove2"	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> insert not_foo not_bar |> remove not_foo |> to_list = [(foo, bar)]));
	  	"choose" 	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> choose = Some (foo, bar)));
	  	"choose2"	>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> insert not_foo not_bar |> choose = Some (foo, bar)));
	  	"fold" 		>:: (fun _ -> assert TestDictionary.(empty |> insert foo bar |> fold (fun k v init -> k + String.length v + init) 0 = 3));
	  	"fold2" 	>:: (fun _ -> assert TestDictionary.(empty |> insert not_foo not_bar |> fold (fun k v init -> k + String.length v + init) 0 = 4));
		]
end

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)

module ListDictionaryTester = DictTester(MakeListDictionary)

let tests = ListDictionaryTester.tests

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)