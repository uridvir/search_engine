open OUnit2
open Data

module type Tests = sig
  val tests : OUnit2.test list
end

module IntKey = struct
	type t = int
	let compare a b =
		if a < b then 'LT
		else if a = b then 'EQ
		else 'GT
	include Formattable with type t := t
end

module StringValue = struct
	type t = string
	let format fmt t =
		Format.fprintf fmt "%s" t
end

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M: DictionaryMaker) = struct
	module TestDictionary = M(IntKey)(StringValue)
	let foo = 0 and bar = "bar" and foobar = [(foo, bar)]
	let tests =
	[
  	"empty" 	>:: fun _ -> assert TestDictionary.empty = [];
  	"is_empty" 	>:: fun _ -> assert TestDictionary.(is_empty []);
  	"size" 		>:: fun _ -> assert TestDictionary.(size []) = 0;
  	"size2"		>:: fun _ -> assert TestDictionary.(size foobar) = 1;
  	"insert" 	>:: fun _ -> assert TestDictionary.(insert foo bar []) = foobar;
  	"member" 	>:: fun _ -> assert TestDictionary.(member foo foobar);
  	"find" 		>:: fun _ -> assert TestDictionary.(find foo foobar) = Some bar;
  	"find2" 	>:: fun _ -> assert TestDictionary.(find 1 foobar) = None;
  	"remove" 	>:: fun _ -> assert TestDictionary.(remove foo foobar) = [];
  	"choose" 	>:: fun _ -> assert TestDictionary.(choose foobar) = Some (foo, bar);
  	"fold" 		>:: fun _ -> assert TestDictionary.(fold (fun k v init -> k + String.length v + init) 0 foobar) = 3;
  	"to_list" 	>:: fun _ -> assert TestDictionary.(to_list foobar) = foobar;
	]
end

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
let tests = []

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)