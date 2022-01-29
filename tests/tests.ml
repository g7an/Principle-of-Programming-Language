
open OUnit2
open Assignment

(*
  This file contains a few tests but not necessarily complete coverage.  You are
   encouraged to think of more tests if needed for the corner cases.
   We will cover the details of the test syntax but with simple copy/paste it should
   not be hard to add new tests of your own without knowing the details.
   1) Write a new let which performs the test, e.g. let test_gcd_2 _ = ...
   2) Add that let-named entity to one of the test suite lists such as section1_tests
      by adding e.g.
       "GCD 2"       >:: test_gcd_2;

   Thats it! OR, even easier, just add another `assert_equal` to the existing tests.
   They will not be submitted with the rest of your code, so you may alter this file as you wish.

   Recall that you need to type "dune test" to your shell to run the test suite.
*)

let test_count_occurrences _ =
  assert_equal (count_occurrences 0 []) 0;
  assert_equal (count_occurrences 1 [1;1;1;1;1]) 5;
  assert_equal (count_occurrences 2 [1;3;5;2;4;2]) 2

let test_reverse_n _ =
  assert_equal (reverse_n 1 []) [];
  assert_equal (reverse_n 1 [1;2;3;4;5]) [1;2;3;4;5];
  assert_equal (reverse_n 3 [5;3;1;7;9]) [1;3;5;7;9]

let test_partition _ =
  assert_equal (partition [true; false; true; false; false] (fun b -> not b)) ([false; false; false], [true; true]);
  assert_equal (partition [1;2;1;1;2;2;1;2;1] (fun n -> (n mod 2) = 0)) ([2;2;2;2], [1;1;1;1;1])

let test_slice _ =
  assert_equal (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i"] 2 7 2) (["c"; "e"; "g"]);
  assert_equal (slice [0;1;2;3;4;5;6;7;8;9] 0 1 3) ([0])


let test_list_comprehension _ =
  assert_equal (list_comprehension [] (fun x -> x mod 11 = 0) (fun x -> x mod 5)) [];
  assert_equal (list_comprehension ["A";"B";"C";"D";"E";"F";"G";"H";"I"] (fun x -> List.mem x (["A";"E";"I";"O";"U"])) (fun a -> a)) ["A";"E";"I"]

let section1_tests =
  "Section 1" >: test_list [
    "Count Occurrences" >:: test_count_occurrences;
    "Reverse n" >:: test_reverse_n;
    "Partition" >:: test_partition;
    "Slice"     >:: test_slice;
    "List Comprehension" >:: test_list_comprehension;
  ]
;;

let test_good_grid_1 =
  [[0;0;1;1]; [1;0;0;0]; [1;0;1;0]; [0;1;0;0]]
;;
(* row_sum: [7;1;4;2]
   col_sum: [5;4;4;1]
*)

let test_bad_grid_1 =
  [[0;0;1;1]; [1;1;0;0]; [1;0;0;0]; [0;1;1;0]]
;;

let test_nth _ =
  assert_equal (nth [0] 1) 0;
  assert_equal (nth [[1;2];[3;4];[5;6]] 2) [3;4]

let test_fetch_column _ =
  assert_equal (fetch_column test_good_grid_1 1) [0;1;1;0];
  assert_equal (fetch_column test_bad_grid_1 2) [0;1;0;1]

let test_verify_list _ =
  assert_equal (verify_list [1; 0; 0; 0; 1; 0; 0; 0] 6) true;
  assert_equal (verify_list [1; 0; 0; 0; 1; 1; 0; 0] 6) false

let test_verify_solution _ =
  assert_equal (verify_solution 4 test_good_grid_1 [7;1;4;2] [5;4;4;1]) true;
  assert_equal (verify_solution 4 test_bad_grid_1 [7;1;4;2] [5;4;4;1]) false

let section2_tests =
  "Section 2" >: test_list [
    "nth" >:: test_nth;
    "Fetch Column" >:: test_fetch_column;
    "Verify List" >:: test_verify_list;
    "Verify Solution"     >:: test_verify_solution;
  ]
;;

let series =
  "Assignment1 Tests" >::: [
    section1_tests;
    section2_tests;
  ]

let () =
  run_test_tt_main series
