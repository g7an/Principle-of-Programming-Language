(*

PoPL Assignment 4 part 2
Your Name : Shuyao Tan
List of Collaborators : Tingyao Li

   For this part, you will write some programs in Fb.  Your answers for
   this section must be in the form of OCaml strings which parse
   to Fb ASTs.  If you want a macro for repeated code, you are welcome to use OCaml
   to put strings together as we did in the `fb_examples.ml` file.
   You are also welcome to copy-paste any code from that file into here.

   Remember to test your Fb code below against the reference Fb binaries (not just
   your own implementation of Fb which could in theory be buggy) to ensure that your
   functions work correctly.

 *)

(*
   Do realize this is a VERY primitive macro system, you will want to put () around
   any definition you make or when appended the parse order could change.

   For questions in this section you are not allowed to use the Let Rec syntax even
   if you have implemented it in your interpreter. Any recursion that you use must
   entirely be in terms of Functions. Feel free to implement an Fb Y-combinator
   here.  For examples and hints, see the file "fbdk/debugscript/fb_examples.ml".

   Remember to test your code against the standard Fb binaries (and not just your own
   implementation of Fb) to ensure that your functions work correctly.

*)

(* Part 2 question 1.

   Fb fails to provide any operations over integers more complex
   than addition and subtraction.  Below, define the following
   2-argument Fb functions: less-than, multiplication, and mod, the
   modulus operator.  (Hint: if you get stuck, try getting them
   working for positive numbers first and then dealing with
   negatives.)  *)

(* compare two variables, a and b, by minus 1 each time. The first variable reaching 0 is the larger one *)
let ycomb = 
   "(Fun code -> 
       Let repl = Fun self -> Fun x -> code (self self) x 
       In repl repl)";;

let is_zero = "(Fun arg -> If arg = 0 Then True Else False)";;
let is_not_positive = ycomb^"(Fun rec -> Fun arg1 -> Fun arg2 -> 
If (arg1 = 0) Then True Else If (arg2 = 0) Then False Else (rec (arg1 + 1) (arg2 - 1)))";;

let code_lt = "(Fun rec -> Fun arg1 -> Fun arg2 -> 
   If (arg1-arg2 = 0) Then False Else If (("^is_not_positive^") (arg1-arg2) (arg1-arg2)) Then True Else False)";;
         
let fb_lt = ycomb^code_lt;;

(* adding arg1 for arg2 times for positive cases. For negative cases, flip sign and then flip back... *)
let flip_sign = "(Fun arg -> If (("^is_not_positive^") arg arg) Then (0 - arg) Else arg)";;
let code_mul = "(Fun rec -> Fun arg1 -> Fun arg2 -> 
   If arg2 = 0 Then 0 Else If (("^is_not_positive^") arg2 arg2) Then ("^flip_sign^") (rec arg1 (("^flip_sign^") (arg2))) Else arg1 + rec arg1 (arg2-1))";;
let fb_mult = ycomb^code_mul ;;
(* modulo operation *)
let code_mod = "(Fun rec -> Fun arg1 -> Fun arg2 -> If ("^fb_lt^") arg1 arg2 Then arg1 Else rec (arg1-arg2) arg2 )";;
let fb_mod = ycomb^code_mod;;
   
(*
   assert(peu ("("^fb_lt^") 33 3") = "False");;
      --> 33 < 3 => False
   assert(peu ("("^fb_lt^") (-1) 3") = "True");;
      --> -1 < 3 => True
   assert(peu ("("^fb_mult^") 5 3") = "15");;
      --> 5 * 3 => 15
   assert(peu ("("^fb_mult^") (-3) 5") = "-15");;
      --> (-3) * 5 => (-15)
   assert(peu ("("^fb_mod^") 33 3") = "0");;
      --> 33 mod 3 => 0
   assert(peu ("("^fb_mod^") 87 4") = "3");;
      --> 87 mod 4 => 3
*)
   
(* Part 2 question 2.

   Fb is a simple language. But even it contains more constructs than strictly necessary.
   For example, you don't even need booleans! They can be encoded using just
   functions using what is called Church's encoding http://en.wikipedia.org/wiki/Church_encoding

   Essentially this encoding allows us to represent booleans as functions. For example:

         True --> Fun t -> Fun f -> t
         False --> Fun t -> Fun f -> f

   We will write five functions that work with church booleans in this section. 
   Remember that all your answers should generate Fb programs as strings.

*)
   
(* To start with let us make OCaml string versions of this Church encoding *)
let church_true = "(Fun t -> Fun f -> t)";;
let church_false = "(Fun t -> Fun f -> f)";;

(* Write a Fb function to convert a church encoded bool to an Fb native bool.*)
let fb_unchurch = "(Fun x -> x True False )";;
   
(* Write a Fb function to convert an Fb native bool to a church encoded bool *)
(* let fb_church = "(Fun bool -> If bool Then ("^church_true^") Else ("^church_false^") )";; *)

let fb_church = "(Fun bool -> If bool Then ("^church_true^") Else ("^church_false^") )";;
(*
   assert (peu (fb_unchurch^"("^fb_church^"(True))") = "True" );;
*)
   
(* Write a function to find out the logical NOT of a church encoded bool *)
(* not = lambda x.x false true
--> not x = x false true = if x then false else true *)
let fbchurch_not = "(Fun x -> If (("^fb_unchurch^") x) Then (("^fb_church^") False) Else (("^fb_church^") True))";;
   
(* Write a function to find out the logical AND of two church encoded bools *)
(* and = lambda x.lambda y.x y false
and x y = if x then y else false *)
let fbchurch_and = "(Fun x -> Fun y -> If (("^fb_unchurch^") x) Then y Else (("^fb_church^") False))";;
   
(* Write a function to find out the logical OR of two church encoded bools *)
(* or = lambda x.lambda y.x true y 
or x y = if x then true else y *)
let fbchurch_or = "(Fun x -> Fun y -> If (("^fb_unchurch^") x) Then (("^fb_church^") True) Else y)";;
   
(* Write a function to find out the logical XOR of two church encoded bools *)
(* xor = lambda x.lambda y.if x then (not y) else y *)
let fbchurch_xor = "(Fun x -> Fun y -> If (("^fb_unchurch^") x) Then (("^fbchurch_not^") y) Else y)";;
   
(* Write a function which will accept an (endoded) boolean and two frozen 
      Fb expressions and will thaw only the appropriate expression. 
      (The reason for freezing the then/else code is if not they would both always run!
      Think about it.)
*)
let thaw e = "(("^e^") 0)";;
let fbchurch_if_then = "(Fun c_bool -> Fun x -> Fun y -> If (("^fb_unchurch^") c_bool) Then "^thaw "x"^" Else "^thaw "y"^")";;
      
(*
   assert(peu (fb_unchurch^"("^fbchurch_not^church_true^")") = "False" );; 
      --> Not True => False
   assert(peu (fb_unchurch^"("^fbchurch_and^church_true^church_false^")") = "False" );;
      --> True And False => False
   assert(peu (fb_unchurch^"("^fbchurch_or^church_true^church_false^")") = "True" );;
      --> True Or False => True
   assert(peu (fb_unchurch^"("^fbchurch_xor^church_true^church_true^")") = "False" );;
      --> True XOr True => False    
   assert(peu (fbchurch_if_then^church_true^"(Fun _ -> 4+1)(Fun _ -> 4-1)") = "5" );;
      --> If True Then 4+1 Else 4-1 => 5 
*)
   