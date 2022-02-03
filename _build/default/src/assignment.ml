(*

PL Assignment 1
 
Name                  : Shuyao Tan
List of Collaborators : Tingyao Li, Mengchu Li

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you will find it helpful to define
auxillary functions, feel free to.

Several of the questions below involve computing well-known mathematical functions;
if you are not familiar with the function named your trusty search engine
should be able to give you an answer, and feel free to ask on Piazza.

You must not use any mutation operations of OCaml for any of these
questions (which we have not taught yet in any case): no arrays,
for- or while-loops, references, etc.

Note for this first assignment you can only use things in standard library Stdlib; 
you **cannot** use list library functions such as `List.hd` or `List.nth` 
(you can code your own versions of them, of course).

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* Here is a simple function which gets passed unit, (), as argument
   and raises an exception.  It is the initial implementation below. *)

let unimplemented () =
	failwith "unimplemented"
	

(* ************** Section One: List operations ************** *)

(* 1a. Write a function that will count the number of occurrences of a specific element
			 in a list. 
*)

let rec count_occurrences elm l = 
   match l with
   | []   -> 0
   | x :: xs -> if x = elm then 1 + count_occurrences elm xs else count_occurrences elm xs
 ;;

(*
# count_occurrences 3 [1;2;3;3;4;5;6] ;;
- : int = 2
# count_occurrences 0 [1;2;3;4;5;6] ;;
- : int = 0
*)

(*
  1b. Write a function to reverse the first n elements of a list. If n is larger that the 
	    number of elements in the list, the entire list should be reversed. 
			You can assume that n >= 0.
*)

(* 
   find the last len(list) - n elements of a list 
*)
let rec last_n n lst =
   match lst with
   |  [] -> []
   |  x :: xs -> if n = 0 then x :: xs else if n = 1 then xs else last_n (n-1) xs
 ;;

(* 
   reverse the first n elements of a list. Truncate the rest of the list.
   Example: 
   reverse_first 3 [1;2;3;4;5;6] ;;
   Details:
   -> (reverse_sub 2 [2;3;4;5;6]) @ [1] -> ((reverse_sub 1 [3;4;5;6]) @ [2]) @ 1 -> 3 @ 2 @ 1 = [3;2;1] 
 *)
let rec reverse_sub n lst =
   match lst with
   |  [] -> []
   |  x :: xs -> if n = 0 then [] else if n = 1 then [x] else reverse_sub (n-1) xs @ [x]
 ;;

 
 let reverse_n n lst = reverse_sub n lst @ last_n n lst ;; 


(*
# reverse_n 3 [1;2;3;4;5;6] ;;
- : int list = [3;2;1;4;5;6]
# reverse_n 3 [1;2;3] ;;
- : int list = [3;2;1]
# reverse_n 3 [1;2;3;4] ;;
- : int list = [3;2;1;4]
# reverse_n 3 [1;2] ;;
- : int list = [2;1]
*)

(* 1c. Sometimes we wish to pick out certain elements in a list that meet a specified
			 condition from the rest, such as getting natural numbers from a list of integers.
			 We can write a general function that will split the list in such a way.
			 Note that the order in the resulting list does not matter; as long as you have the
			 right elements in the respective partition, the function will be considered as correct.
*)

(* consing the element if it meets the condition *)
let rec match_pattern l cond_f = 
   match l with
   |  [] -> []
   |  x :: xs -> if cond_f x then x :: match_pattern xs cond_f else match_pattern xs cond_f
 ;;

(* consing the element if it does not meet the condition *)
let rec unmatch_pattern l cond_f = 
   match l with
   |  [] -> []
   |  x :: xs -> if cond_f x then unmatch_pattern xs cond_f else x :: unmatch_pattern xs cond_f
 ;;


let partition l cond_f = (match_pattern l cond_f,  unmatch_pattern l cond_f) ;;


(*
# partition [-5;-4;-3;-2;-1;0;1;2;3;4;5] (fun n -> n > 0) ;;
- : int list * int list = ([1;2;3;4;5], [-5;-4;-3;-2;-1;0]) 
# partition ["My"; "Name"; "Is"; "Jean"; "Valjean"] (fun s -> String.equal s "Javert") ;;
- : string list * string list = ([], ["My"; "Name"; "Is"; "Jean"; "Valjean"])
*)

(* 
	1d. Now let's try to write a Python-style list operation in Ocaml! List slicing 
			is a common and useful technique: the user will specify where the slice
			starts, where it ends, and the step they wish to take. For simplicity's sake,
			we will assume that we are only working with non-negative indices and positive step.
			
			Notes on corner cases:
			- If fin > length of the list, assume the operation will cover the entire list.
			- If end < init, return [].

*)
   
(* simple version of step function  *)
(* 
let rec step l jump index count = 
   match l with 
   | [] -> []
   | x :: xs -> if index = count then x :: step xs jump (index + 1) (index+jump) else step xs jump (index + 1) count
;; *)

(* use an aux. function with 2 additional arguments, index and count. `count` is a variable that initiates to the value of `init` 
and is incremented by `jump` each time the function is called. In this way, the step functionality can be realized.
We go through the list and return the elements that are between `init` and `fin` (exclusive). *)
let slice l init fin jump = 
   let rec slice_inner l init fin jump index count = 
      match l with
      | [] -> []
      | x :: xs -> if fin = 0 then [] else if init <= 0 && fin > 0 && index = count then x :: slice_inner xs (init-1) (fin-1) jump (index+1) (index+jump) else slice_inner xs (init-1) (fin-1) jump (index+1) count
   in
   slice_inner l init fin jump 0 init
;;


(*
# slice [0;1;2;3;4;5;6;7;8;9] 1 9 2 ;;
- : int list = [1;3;5;7]
# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 0 5 3 ;;
- : string list = ["a";"d"]
*)

(*
  1e. Many programming languages support the idea of a list comprehension - a mechanism
      to construct new lists from existing lists. The syntax is usually based on
      the mathematical set builder notation.

      For example the python expression lst = [ x*x for x in range(10) if x % 2 == 0 ]
      creates a list whose values are squares of even integers between 0 and 10 (exclusive).

      This construct is very functional in nature; so let us define a function - list_comprehension
      to help us with this. It takes 3 parameters - a source list of values (source), a predicate to
      filter values (pred) and a computation function (compute) to create a new value. The output is
      the list of values produced by the compute function for those source values that satisfy the
      predicate. The order of the items in the output list is based on the order in the source list.

      E.g. list_comprehension [0;1;2;3;4;5;6;7;8;9] (fun x -> (x mod 2) = 0) (fun x -> x * x) produces
      the list [0, 4, 16, 36, 64] similar to the python expression above.

*)

let rec list_comprehension source pred compute =
   match source with
   | [] -> []
   | x :: xs -> if pred x then compute x :: list_comprehension xs pred compute else list_comprehension xs pred compute
;;

(*
# let rec range n = match n with 1 -> [0] | x -> (range (n-1)) @ [x-1] ;;
val range : int -> int list = <fun>
# list_comprehension (range 101) (fun x -> (x mod 5) = 0) (fun x -> x / 5) ;;
- : int list =
[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20]
*)

(* ************** Section Two: Kakurasu verifier ************** *)

(* Kakurasu is a Japanese logic puzzle. The goal of this question is
   to write a function that when presented with a kakurasu grid,
   returns whether it's been correctly solved. In case you're not
   familiar with the game, the rules and some examples can be found here:
   https://puzzlemadness.co.uk/kakurasu/medium#rules

   To verify a grid, we have to check that each row/column's filled out
   squares have combined value that is equal to the desired amount.

   For example, take a look at the second row in the example below:
   since the fourth, fifth, and eighth squares are marked as 1,
   we have 4 + 5 + 8 = 17, and it matches the expected row value on
   the right.

   Similarly, column one has the first, fourth, sixth, and seventh
   squares filled out, giving us 1 + 4 + 6 + 7 = 18.

      1  2  3  4  5  6  7  8
   1  ■  -  -  -  -  -  -  - -- 1
   2  -  -  -  ■  ■  -  -  ■ -- 17
   3  -  ■  ■  -  -  ■  -  - -- 11
   4  ■  -  ■  ■  ■  ■  ■  ■ -- 34
   5  -  -  ■  -  -  -  ■  ■ -- 18
   6  ■  -  -  -  -  ■  ■  ■ -- 22
   7  ■  -  -  -  -  -  ■  ■ -- 16
   8  -  -  -  ■  ■  ■  -  ■ -- 23
      |  |  |  |  |  |  |  |
     18  3 12 14 14 21 22 32

   A simple representation of an n x n Kakurasu grid is as a list of length n,
   each of the n element is itself a length-n list. We can use 0 to represent
   unfilled squares, and 1 for filled ones.

   e.g.: The above example will be represented as the list
   [[1; 0; 0; 0; 0; 0; 0; 0]; 
    [0; 0; 0; 1; 1; 0; 0; 1];
    [0; 1; 1; 0; 0; 1; 0; 0]; 
    [1; 0; 1; 1; 1; 1; 1; 1];
    [0; 0; 1; 0; 0; 0; 1; 1]; 
    [1; 0; 0; 0; 0; 1; 1; 1];
    [1; 0; 0; 0; 0; 0; 1; 1]; 
    [0; 0; 0; 1; 1; 1; 0; 1]]

   The function itself will take in four arguments: the dimension (represented by
   a single integer n for an n x n grid), the Kakurasu grid, a list containing
   desired sums for each row, and a list containing desired sums for each column.
*)

(* 2a. Since we will need to query a specific row from the desired sums list,
       getting a row value by index will come in handy. Note we will use 1-based
       indexing for convenience.
 *)

let rec nth lst n = 
   match lst with
   | [] -> failwith "Empty list or invalid index"
   | x :: xs ->  if n = 1 then x else nth xs (n-1)
 ;;


(*
# nth [1;0;0;0;0;0;0;0] 1 ;;
- : int = 1
# nth [1;0;0;0;0;0;0;0] 5 ;;
- : int = 0
*)


(* 2b. The list representation makes it easy to access each row, but it can be
       tricky to get the columns. For this question, write a function that when
       given a column index (1-based) and a grid, extracts that specified column
			 as an integer list.
 *)

let rec fetch_column grid col = 
   match grid with
   | [] -> []
   | x :: xs -> nth x col :: fetch_column xs col
;;

(*
# let test_grid = 
	 [[1; 0; 0; 0; 0; 0; 0; 0]; 
    [0; 0; 0; 1; 1; 0; 0; 1];
    [0; 1; 1; 0; 0; 1; 0; 0]; 
    [1; 0; 1; 1; 1; 1; 1; 1];
    [0; 0; 1; 0; 0; 0; 1; 1]; 
    [1; 0; 0; 0; 0; 1; 1; 1];
    [1; 0; 0; 0; 0; 0; 1; 1]; 
    [0; 0; 0; 1; 1; 1; 0; 1]]
	;;
# fetch_column test_grid 1 ;;
- : int list = [1; 0; 0; 1; 0; 1; 1; 0]
# fetch_column test_grid 7 ;;
- : int list = [0; 0; 0; 1; 1; 1; 1; 0]
*)

(* 2c. Write a function that, given a list and a desired sum, verifies that
       the combined value is equal to the sum.
			 e.g. verify_list [0; 0; 0; 1; 1; 1; 0; 1] 23 = true;;
			 			verify_list [1; 0; 0; 0; 1; 0; 0; 0] 19 = false;;
*)

let verify_list lst sum = 
   let rec calculate_sum lst sum index counter = 
      match lst with
         | [] -> if counter = sum then true else false
         | x :: xs -> if x = 0 then calculate_sum xs sum (index+1) counter else calculate_sum xs sum (index+1) (counter+index)
   in 
      calculate_sum lst sum 1 0
;;


(*
# verify_list [0; 0; 0; 1; 1; 1; 0; 1] 23 ;;
- : bool = true
# verify_list [1; 0; 0; 0; 1; 0; 0; 0] 19 ;;
- : bool = false
*)

(* 2d. Now we can put it all together and verify the entire grid! *)

let rec verify_solution_row grid row_vals index = 
   match grid with
   | [] -> true
   | x :: xs -> 
      if verify_list x (nth row_vals index) then verify_solution_row xs row_vals (index+1) else false
;;

let rec verify_solution_col dimension grid fetch_column col_vals cnt = 
   if cnt <= dimension then verify_list (fetch_column grid cnt) (nth col_vals cnt) && (verify_solution_col dimension grid fetch_column col_vals (cnt+1)) else true
;;

let verify_solution dimension grid row_vals col_vals =
   (verify_solution_row grid row_vals 1) && (verify_solution_col dimension grid fetch_column col_vals 1)
;;



(*
# verify_solution 8 test_grid [1;17;11;34;18;22;16;23] [18;3;12;14;14;21;22;32];;
- : bool = true
*)
