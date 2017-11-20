open OUnit2
open Engine

let index_of_dir_test _ =
	let expected = [("Hello", ["helloworld.txt"]); ("World", ["helloworld.txt"])] in
	let result = ListEngine.(index_of_dir (Sys.getcwd () ^ "/../../src/test") |> to_list) in
	assert (result = expected)

let tests = 
	[
		"index_of_dir" >:: index_of_dir_test;
	]

(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)