open OUnit2
open Data

module type Tests = sig
  val tests : OUnit2.test list
end

(* [DictTester] is where you will implement your test harness
 * to find buggy implementations. *)
module DictTester (M:DictionaryMaker) = struct
  let tests = []
end

(* [tests] is where you should provide OUnit test cases for
 * your own implementations of dictionaries and sets.  You're
 * free to use [DictTester] as part of that if you choose. *)
let tests = []

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)