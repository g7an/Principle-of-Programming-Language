(*

PL Assignment 2
 
Name                  : 
List of Collaborators :

Please make a good faith effort at listing people you discussed any 
problems with here, as per the course academic integrity policy.  
CAs/Prof need not be listed!

Fill in the function definitions below replacing the 

  unimplemented ()

with your code.  Feel free to add "rec" to any function listed to make
it recursive. In some cases, you will find it helpful to define
auxillary functions, feel free to.

You must not use any mutation operations of OCaml for any of these
questions: no arrays, for- or while-loops, references, etc.

Note that you *can* use List.map etc library functions on this homework.

*)

(* Disables "unused variable" warning from dune while you're still solving these! *)
[@@@ocaml.warning "-27"]

(* Here is again our unimplemented "BOOM" function for questions you have not yet answered *)

let unimplemented _ =
	failwith "unimplemented"

(* ************** Section 1: Thinking like a type inferencer ************** *)

(* 
  To better understand OCaml's parametric types, you are to write functions that 
  return the indicated types.  
  Note you can ignore the lists of type variables at the front of the types in 
  the below, e.g. the `'a 'b 'c 'd.` in the `f1` type; those are OCaml notation 
  meaning those types *have* to be polymorphic.  We have answered the first question 
  for you to make it clear.
*)

let f0 : 'a. 'a -> int = (fun _ -> 4) (* answered for you *)

let f1 : 'a. 'a -> 'a -> 'a * 'a = (fun x y -> (x, y)) 

let f2 : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c) = (fun f g x -> g (f x)) ;;


(* why fun f () a -> f () a cannot be working? *)
let f3 : 'a 'b. (unit -> 'a -> 'b) -> 'a -> (unit -> 'b) = (fun f a ()  -> f () a) ;;

let f4 : 'a 'b. 'a -> ('b option) list = (fun x -> None::[]) ;;

let f5 : 'a 'b. ('a -> 'b) list -> 'a list -> 'b list = (fun _ _ -> []) ;;

let rec func f a = 
  match f with         
  | [] -> []
  | x::lr -> x (List.hd a):: func lr a
;;


type ('a, 'b) sometype = 
  | Left of 'a 
  | Right of 'b
;;

let f6 : 'a 'b 'c. ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) sometype -> 'c = (fun f g x -> 
  match x with
  | Left a  -> f a
  | Right b -> g b
  )  ;;

(* destruct variant into two list of types *)
let rec f7 : 'a 'b. (('a, 'b) sometype) list -> ('a list * 'b list) = 
  (fun l -> 
    match l with
    | [] -> ([], [])
    | x::lr -> 
      let (l1, l2) = f7 lr in
      match x with
      | Left a -> (a::l1, l2)
      | Right b -> (l1, b::l2)
  )
    ;;


let rec destruct = 
  (fun l -> 
    match l with
    | [] -> ([], [])
    | x::lr -> 
      let (l1, l2) = destruct lr in
      match x with
      | Left a -> (a::l1, l2)
      | Right b -> (l1, b::l2))
    ;;
  
  (* (fun _ -> ([], [])) ;; *)
  

(* ************** Section 2 : An ode to being lazy ************** *)

(* 
  Lazy programming languages don't evaluate components of lists so it is possible to 
  write an "infinite" list via OCaml pseudo-code such as

  let rec mklist n = n :: (mklist (n+1));;
  mklist 0;;

  Now, if you typed this into OCaml the `mklist 0` would unfortunately loop forever.

  **But**, if you "froze" the computation of the tail of the list by making it a 
  function, we can achieve something in this lazy spirit.

  We will give you the type of lazy sequences to help you get going on this question:
*)

type 'a sequence =
  | Nil 
  | Sequence of 'a * (unit -> 'a sequence)

(* 
  This is similar to the list type; the difference is the `unit ->` which keeps 
  the tail of the list from running by making it a function.
  Here for example is how you would write an infinite sequence of zeroes:
*)

let rec zeroes = 
  Sequence(0, fun () -> zeroes)

(* Here is a dummy sequence used in templates below 
   - remove and replace with your answer. *)  

let rec unimplemented_int_sequence = 
  Sequence(0, fun () -> unimplemented_int_sequence ) ;;

(* 
  It is still possible to express finite lists as sequences,
  for example here is [1; 2] as a sequence, with `Nil` denoting the empty sequence.
*)

let one_and_two = 
  Sequence(1, fun () -> Sequence(2, fun () -> Nil)) ;;

(*
  Write a function to convert a sequence to a list. Of course if you try to 
  evaluate this on an infinite sequence such as `zeroes` above, it will not finish. 
  But we will assume sanity on the caller's part and ignore that issue in this question.  
*)

let rec list_of_sequence (s: 'a sequence) : 'a list = 
  match s with
  | Nil -> []
  | Sequence(x, f) -> x::list_of_sequence (f ()) ;;

(* 
# list_of_sequence one_and_two ;;
- : int list = [1; 2] 
*)

(*
  While it is nice to have these infinite sequences, it is often useful to "cut" 
  them to a fixed size. Write a function that cuts off a sequence after a fixed 
  non-negative number of values `n`, producing a new, potentially shorter sequence.
  
  (Treat the given count `n` as the maximum number of elements allowed in the 
  output sequence. So if the input is a finite sequence and its length is less
  than the specified count, the output sequence can have less than `n` values)
*)

let rec cut_sequence (n : int) (s: 'a sequence) : 'a sequence = 
  match s with
  | Nil -> Nil
  | Sequence(x, f) -> 
    if n = 0 then Nil
    else Sequence(x, fun () -> cut_sequence (n-1) (f ()) ) ;;

(* 
# list_of_sequence (cut_sequence 5 zeroes) ;;
- : int list = [0; 0; 0; 0; 0] 
*)

(* Write a function to get the nth element of the sequence.
   Note: We're using 0-based indexing. Throw an exception if out of bound. *)

exception Out_of_bound of string ;;
let rec nth_sequence (n : int) (s : 'a sequence) : 'a = 
  match s with
  | Nil -> raise (Out_of_bound "Out of bound")
  | Sequence(x, f) -> 
    if n = 0 then x
    else nth_sequence (n-1) (f ()) ;;
;;
  


(* 
# nth_sequence 0 one_and_two ;;
- : int = 1
# nth_sequence 1 one_and_two ;;
- : int = 2 
*)

(* Now, write a fold_left function for sequence! *)

let rec fold_left_sequence (folder : 'a -> 'b -> 'a) (acc : 'a) (s : 'b sequence) = 
  match s with
  | Nil -> acc
  | Sequence(x, f) -> fold_left_sequence folder (folder acc x) (f ()) ;;


(* 
# fold_left_sequence (+) 0 one_and_two ;;
- : int = 3
*)

(*
  Write a mapi function (analogous to List.mapi). This function is very similar to
  map, and the only different is that the mapper itself now takes the current index
  as an argument as well. You can find a more detailed documentation here:
  https://ocaml.org/api/List.html

  Note: We're using 0-based indexing in this function.
*)

let mapi_sequence (fn : int -> 'a -> 'b) (s : 'a sequence) : 'b sequence =
  let rec loop (i : int) (s : 'a sequence) : 'b sequence = 
    match s with
    | Nil -> Nil
    | Sequence(x, f) -> Sequence(fn i x, fun () -> loop (i+1) (f ()) ) 
  in loop 0 s ;;


(* 
# list_of_sequence (mapi_sequence (fun i -> fun x -> i + x) one_and_two) ;;
- : int list = [1; 3] 
# list_of_sequence (mapi_sequence (fun i -> fun _ -> i * i) one_and_two) ;;
- : int list = [0; 1] 
*)

(*
  Now, let's write an infinite sequences that represent triangle numbers!
  (see https://en.wikipedia.org/wiki/Triangular_number for more information on
   triangle numbers)
*)

(* let rec unimplemented_int_sequence = 
  Sequence(0, fun () -> unimplemented_int_sequence ) ;; *)
let triangles : int sequence = 
  let rec helper_traingles (n : int) : int sequence = 
    Sequence(n*(n+1)/2, fun () -> helper_traingles (n+1) )
  in helper_traingles 0 ;;

(* 
# list_of_sequence (cut_sequence 10 triangles) ;;
- : int list = [0; 1; 3; 6; 10; 15; 21; 28; 36; 45] 
*)

(* 
  Now let's use the triangle sequence we have to write a sequence that 
  represents tetrahedral numbers. The nth tetrahedral number is the sum of 
  the first n triangular numbers. 
  For more information, see https://en.wikipedia.org/wiki/Triangular_number#Formula
*)


let tetrahedrals : int sequence = 
  let rec helper_tetrahedrals (n : int) : int sequence = 
    Sequence(fold_left_sequence (+) 0 (mapi_sequence (fun i x -> x) (cut_sequence n triangles)) , 
      fun () -> helper_tetrahedrals (n+1) )
  in helper_tetrahedrals 1 ;;



(* 
# nth_sequence 9 (cut_sequence 10 tetrahedrals) ;;
- : int = 165 
*)


(* ****************** Section 3: Spice up your life! ************************ *)
(* 
  Imagine for a moment that you're a beginner in the world of spicy food. You're
  deeply curious, but also terrified at the same time. As a person of science,
  you decide to approach this in a scientific manner.

  Here's how you set up your experiment:
  - There are six different types of pepper you can add to your dish.
  - You will use one type of pepper in your cooking at a time.
  - Each time you use a type of pepper, you'll put it to the front of your spice 
    drawer. 
  - You also keep a record of how many times you've used a particular type of pepper.
  - Rinse & repeat many times...
  - Analyze the accumulated data as well as your drawer after an extended period of
    time.
*)

(*
  To accomplish this task, we will be using a data structure called association list.
  It is essentially like a list-based encoding of maps, where each element in the 
  list is in the form of a key-value pair, (k, v). We can use k to look up a pair and 
  get its corresponding v. 
*)

(* We have provided the type declaration for the association list type. *)

type ('a, 'b) alist = ('a * 'b) list

(* 
  Now, write a basic function that will return the value in the pair when given the 
  key. Note that this function returns an option type; it allows you to handle errors 
  like missing keys in a more type-correct way.
*)

let assoc (k : 'a) (al : ('a, 'b) alist) : 'b option = 
  unimplemented ()

(* 
# assoc "Dog" ([("Dog", 4); ("Cat", 42)]) ;;
- : int option = Some 4
# assoc "Mr. Garak" [("Mr. Garak", "Plain, simple Tailor"); ("Dr. Bashir", "Chief Medical Disaster"); ("Chief O'Brien", "Must Suffer")];;
- : string option = Some "Plain, simple Tailor"
*)

(* 
  Write a function that when given a key will tell you whether there is an associated 
  pair in the alist.
*)

let exists (k : 'a) (al : ('a, 'b) alist) : bool = 
  unimplemented ()

(* 
# exists "Kitten" ([("Kitten", 13); ("Cat", 42)]) ;;
- : bool = true
# exists 11 ([(7, "Keep your ears open."); (35, "Peace is good for business."); 
            (286, "When Morn leaves, it's all over.")]) ;;
- : bool = false
*)

(* 
  Let's write a function that will update a (k, v) pair in your alist when given a new
  mapping. If key doesn't already exist in the alist, add the pair to the front of the
  list. If there are multiple mappings of k, modify the first pair you encounter in 
  the alist.
*)

let update (key : 'a) (v' : 'b) (al : ('a, 'b) alist) : ('a, 'b) alist = 
  unimplemented ()

(* 
# update "Dukat" "Evil incarnate" [("Kira", "Bajoran"); ("Sisko", "Human?"); ("Odo", "Goo")];;
- : (string, string) alist = 
  [("Dukat", "Evil incarnate"); ("Kira", "Bajoran"); ("Sisko", "Human?"); ("Odo", "Goo")]
*)

(* Now we're ready to dive into The Spicy Part! *)

(* 
  We have defined the data type that represents the six different types of pepper you
  will be using in the experiment, as well as the starting state of your drawer.
*)

type pepper = BellPepper | Poblano | Cayenne | Habanero | GhostPepper | PepperX

let init_drawer = 
  [(BellPepper, 0); (Poblano, 0); (Cayenne, 0); 
   (Habanero, 0); (GhostPepper, 0); (PepperX, 0)]

(* 
  This will be your main cooking routine.
  Whenever you use a type of pepper, you need to promote it to the front of the list 
  and add 1 to the number of times used. Remember, there shouldn't be any duplicates in 
  your drawer!
*)

let use_pepper (p : pepper) (drawer : (pepper, int) alist) : (pepper, int) alist = 
  unimplemented ()

(* 
# use_pepper Cayenne init_drawer ;;
- : (pepper, int) alist = 
    [(Cayenne, 1); (BellPepper, 0); (Poblano, 0);
     (Habanero, 0); (GhostPepper, 0); (PepperX, 0)]
*)

(* 
  Now, let's find out how much of a spicy food lover you are :)
  Write a function that will calculate the "spicy score" when given a spice drawer.
  The score is simply calculated by the following equation:
  spicy_score = sum of (pepper_score * times_used)

  FYI: The pepper scores are given below:
       Bell pepper - 10; Poblano - 20; Cayenne - 35; 
       Habanero - 50; Ghost pepper - 60; PepperX - 80 
*)

let spicy_score (drawer : (pepper, int) alist) : int = 
  unimplemented ()

(* 
# spicy_score (use_pepper Cayenne init_drawer) ;;
- : int = 35
# spicy_score (use_pepper Cayenne (use_pepper Habanero (use_pepper Cayenne init_drawer))) ;;
- : int = 120
*)

(*
  You might have noticed that we decide to always put the last used pepper to the front of
  the drawer. However, is this always a good idea? Ideally, we want our drawer to be sorted
  in the order of descending frequency of use, i.e.: we will have the most frequently used 
  pepper in the very front, followed by the second most frequently used, and so on. To evaluate
  our placement strategy, write a function that measures the sortedness of your drawer by 
  determining the radius for your k-sorted drawer. The radius is defined as the maximum 
  absolute difference between an element's actual index in the list vs. its index in the sorted
  list.

  e.g. The list [1; 2; 3; 0] is a 3-sorted list.
  
  (For more information on k-sorted sequence, see https://en.wikipedia.org/wiki/K-sorted_sequence)
*)

let is_good_drawer_arrangement (drawer : (pepper, int) alist) : int = 
  unimplemented () 

(* 
# is_good_drawer_arrangement (use_pepper Cayenne init_drawer) ;;
- : int = 0
# is_good_drawer_arrangement (use_pepper Habanero (use_pepper Cayenne (use_pepper Cayenne init_drawer))) ;;
- : int = 1
*)

(*************** Section 4: Mutable State and Memoization ******************)

(* Note: You will need to use mutable state in some form for questions in this section *)

(* 
  Cache: Pure functions (those without side effects) always produces the same value
  when invoked with the same parameter. So instead of recomputing values each time,
  it is possible to cache the results to achieve some speedup.

  The general idea is to store the previous arguments the function was called
  on and its results. On a subsequent call if the same argument is passed, 
  the function is not invoked - instead, the result in the cache is immediately 
  returned.  

  Note: For this question you don't have to worry about the case of using the cache
  for recursive calls. i.e. if you have a function, cached_factorial, we won't expect
  your function to look at the cache in the smaller recursive calls. 

  e.g. let _ = cached_factorial 1 in
       let _ = cached_factorial 3 in
       cached_facotiral 5

  doesn't invoke the cache, although technically 3 and 5 can both use previous computation
  to inform their calculations.
*)

(*
  Given any function f as an argument, create a function that returns a
  data structure consisting of f and its cache.
*)  
let new_cached_fun f = unimplemented ()

(*
  Write a function that takes the above function-cache data structure,
  applies an argument to it (using the cache if possible) and returns
  the result.
*)
let apply_fun_with_cache cached_fn x = unimplemented ()

(*
  The following function makes a cached version for f that looks
  identical to f; users can't see that values are being cached 
*)

let make_cached_fun (f : 'a -> 'b) : 'a -> 'b = 
  let cf = new_cached_fun f in 
    function x -> apply_fun_with_cache cf x

(*
let f x = x + 1 ;;
let cache_for_f = new_cached_fun f ;;
apply_fun_with_cache cache_for_f 1 ;;
cache_for_f ;;
apply_fun_with_cache cache_for_f 1 ;;
cache_for_f ;;
apply_fun_with_cache cache_for_f 2 ;;
cache_for_f ;;
apply_fun_with_cache cache_for_f 5 ;;
cache_for_f ;;
let cf = make_cached_fun f ;;
cf 4 ;;
cf 4 ;;


# val f : int -> int = <fun>
# val cache_for_f : ... 
# - : int = 2
# - : ...
# - : int = 2
# - : ...
# - : int = 3
# - : ...
# - : int = 6
# - : ...
# val cf : int -> int = <fun>
# - : int = 5
# - : int = 5
*)