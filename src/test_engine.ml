open OUnit2
open Engine

let sanitize_directory dir =
	String.escaped (String.map (fun c -> if c = ' ' then '\ ' else c) dir)

module EngineTester (E: Engine) = struct
	let sanitized_test_directory = sanitize_directory (Sys.getcwd () ^ "/../../src/test")

	let index_of_dir_test _ =
		let expected = [("Hello", ["helloworld.txt"]); ("World", ["helloworld.txt"])]
		and result = E.(index_of_dir sanitized_test_directory |> to_list) in
		assert (result = expected)

	let and_not_test _ =
		let expected = ["helloworld.txt"]
		and result = E.(index_of_dir sanitized_test_directory |> and_not ["Hello"; "World"] ["foo"; "bar"]) in
		assert (result = expected)

	let or_not_test _ =
		let expected = ["helloworld.txt"]
		and result = E.(index_of_dir sanitized_test_directory |> or_not ["Hello"; "darkness"; "my"; "old"; "friend"]
      ["foo"; "bar"]) in
		assert (result = expected)

	let tests =
		[
			"index_of_dir" 	>:: index_of_dir_test;
			"and_not"		>:: and_not_test;
			"or_not"		>:: or_not_test;
		]
end

module ListEngineTester = EngineTester(ListEngine)
module TreeEngineTester = EngineTester(TreeEngine)

let tests = ListEngineTester.tests @ TreeEngineTester.tests
(* DO NOT call OUnit2.run_test_tt_main from here.  It must
 * be called only in test_main.ml.  *)
