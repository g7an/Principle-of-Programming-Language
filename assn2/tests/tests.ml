
open OUnit2
open Assignment

(*
  This file contains a few tests but not necessarily complete coverage.  You are
   encouraged to think of more tests if needed for the corner cases.
   We will cover the details of the test syntax but with simple copy/paste it should
   not be hard to add new tests of your own without knowing the details.
   1) Write a new let which performs the test, e.g. let test_assoc _ = ...
   2) Add that let-named entity to one of the test suite lists such as section1_tests
      by adding e.g.
       "Assoc"       >:: test_assoc;

   Thats it! OR, even easier, just add another `assert_equal` to the existing tests.
   They will not be submitted with the rest of your code, so you may alter this file as you wish.

   Recall that you need to type "dune test" to your shell to run the test suite.
*)

let test_function_types _ =
  let _ = f1 true false in
  let _ = f2 int_of_string string_of_int "1" in
  let _ = f3 (fun () n -> n > 0) 1 () in
  let _ = f4 [] in
  let _ = f5 [(fun n -> n + 1); (fun n -> n * 2)] [1;2;3;4] in
  let _ = f6 (fun n -> n > 0) (fun b -> not b) (Left 42) in
  let _ = f7 [Left 1; Right true] in
  ()

let section1_tests =
  "Section1" >::: [
    "FunctionTypes" >:: test_function_types;
  ]

let test_list_of_sequence _ =
  assert_equal [1;2]      (list_of_sequence one_and_two);
  assert_equal [0;0;0;0;0] (list_of_sequence (cut_sequence 5 zeroes));
  assert_equal [0;1;3;6] (list_of_sequence (cut_sequence 4 triangles));
  assert_equal [0;1;4;10]  (list_of_sequence (cut_sequence 4 tetrahedrals))

let test_triangles _ =
  assert_equal [0;1;3;6;10;15;21;28;36;45]
    (list_of_sequence (cut_sequence 10 triangles))

let test_tetrahedrals _ =
  assert_equal [0;1;4;10;20;35;56]
    (list_of_sequence (cut_sequence 7 tetrahedrals))

let test_mapi_sequence _ =
  assert_equal [0;1;9;36;100]
    (cut_sequence 5 triangles
    |> mapi_sequence (fun _ -> fun x -> x * x)
    |> list_of_sequence);
  assert_equal "3:10"
    (cut_sequence 5 tetrahedrals
    |> mapi_sequence (fun i -> fun x -> string_of_int i ^ ":" ^ string_of_int x)
    |> cut_sequence 4
    |> nth_sequence 3)

let test_fold_left_sequence _ =
  assert_equal 20
    (cut_sequence 5 triangles
    |> fold_left_sequence (+) 0);
  assert_equal 34
    (cut_sequence 5 tetrahedrals
    |> fold_left_sequence
       (fun acc n -> if n mod 2 = 0 then (n + acc) else acc) 0)

let section2_tests =
  "Section2" >::: [
    "ListOfSequence" >:: test_list_of_sequence;
    "Triangles"      >:: test_triangles;
    "Tetrahedrals"   >:: test_tetrahedrals;
    "MapiSequence"   >:: test_mapi_sequence;
    "FoldLeftSequence" >:: test_fold_left_sequence;
  ]

let test_alist _ =
  assert_equal false
    ([("Obi-wan", "Padawan")]
     |> update "Obi-wan" "Jedi Master"
     |> update "Anakin" "Padawan"
     |> exists "Qui-Gon Jinn"
     );
  assert_equal (Some "Darth Vadar")
    ([("Obi-wan", "Jedi Master"); ("Anakin", "Jedi Master"); ("Ahsoka", "Padawan")]
     |> update "Palpatine" "Sith"
     |> update "Anakin" "Darth Vadar"
     |> assoc "Anakin"
     )

let init_drawer =
  [(BellPepper, 0); (Poblano, 0); (Cayenne, 0);
    (Habanero, 0); (GhostPepper, 0); (PepperX, 0)]

let test_spicy _ =
  assert_equal 160
    (init_drawer
     |> use_pepper Cayenne
     |> use_pepper Cayenne
     |> use_pepper PepperX
     |> use_pepper BellPepper
     |> spicy_score
    );
  assert_equal 3
    (init_drawer
     |> use_pepper Cayenne
     |> use_pepper Cayenne
     |> use_pepper PepperX
     |> use_pepper Poblano
     |> use_pepper BellPepper
     |> is_good_drawer_arrangement
    );
  assert_equal 0
  (init_drawer
    |> use_pepper Cayenne
    |> use_pepper Poblano
    |> use_pepper Cayenne
    |> is_good_drawer_arrangement
  )

let section3_tests =
  "Section3" >::: [
    "AssociationList"   >:: test_alist;
    "SpicyTest"   >:: test_spicy;
  ]

let square_of n = n * n

let dec n = n - 1

(* NOTE: Delete these two lines once you finish implementing Section 4 *)
let cached_square_of _ = unimplemented ()
let cached_dec _ = unimplemented ()

(* Note: Use these two functions instead to test your Section 4 answer *)
(* let cached_square_of = make_cached_fun square_of
let cached_dec = make_cached_fun dec *)

let test_cached_fun _ =
  assert_equal 9
  (let _ = cached_square_of 1 in
   let _ = cached_square_of 2 in
   let _ = cached_square_of 3 in
   let _ = cached_square_of 4 in
   cached_square_of 3);
  assert_equal 0
  (let _ = cached_dec 1 in
  let _ = cached_dec 2 in
  let _ = cached_dec 1 in
  let _ = cached_dec 4 in
  cached_dec 1)

let section4_tests =
  "Section4" >::: [
    "CachedFun"   >:: test_cached_fun;
  ]

let series =
  "Assignment" >::: [
    section1_tests;
    section2_tests;
    section3_tests;
    section4_tests;
  ]

let () =
  run_test_tt_main series
