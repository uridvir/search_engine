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
	let foobar = [(0, "bar")]
	let compare_dictionary (lit: (int * string) list)(dic: TestDictionary.t) : bool =
		try
			List.for_all2 (fun (a: int * string) (b: TestDictionary.key * TestDictionary.value) -> let (k1, v1) = a and (k2, v2) = b in k1 = k2 && v1 = v2) lit dic
		with
		| Invalid_argument -> false	
	let tests =
		[
	  	"empty" 	>:: fun _ -> assert TestDictionary.(empty = []);
	  	"is_empty" 	>:: fun _ -> assert TestDictionary.(is_empty empty);
	  	"size" 		>:: fun _ -> assert TestDictionary.(size empty = 0);
	  	"size2"		>:: fun _ -> assert TestDictionary.(size foobar = 1);
	  	"insert" 	>:: fun _ -> assert TestDictionary.(insert foo bar empty = foobar);
	  	"member" 	>:: fun _ -> assert TestDictionary.(member foo foobar);
	  	"find" 		>:: fun _ -> assert TestDictionary.(find foo foobar = Some bar);
	  	"find2" 	>:: fun _ -> assert TestDictionary.(find 1 foobar = None);
	  	"remove" 	>:: fun _ -> assert TestDictionary.(remove foo foobar = empty);
	  	"choose" 	>:: fun _ -> assert TestDictionary.(choose foobar = Some (foo, bar));
	  	"fold" 		>:: fun _ -> assert TestDictionary.(fold (fun k v init -> k + String.length v + init) 0 foobar = 3);
	  	"to_list" 	>:: fun _ -> assert TestDictionary.(to_list foobar = foobar);
		]
end

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
let tests = []

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)