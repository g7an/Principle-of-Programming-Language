(* To use the reference interpreter, type

   $ ./reference/FbRx/toplevel.exe

To run these examples in your interpeter, type shell command

    $ dune utop ./FbRX
*)
open Fbdk.Ast;;
open Debugutils;;

let ex0 = "Let x = {a=5} In x";;  
(* (Let ((Ident "x"), (Record [((Lab "a"), (Int 5))]), (Var (Ident "x")))) *)
let ex1 = "Let x = {a=5} In x.a";; 
(* 
(Let ((Ident "x"), (Record [((Lab "a"), (Int 5))]),
   (Select ((Lab "a"), (Var (Ident "x")))))) 
*)
let ex2 = "Let x = {size=7; weight=255} In x";;
let ex3 = "Let x = {size=7; weight=255} In x.size";;
let ex4 = "{one = 1; two = 2;
three = 2 + 1; four = (Function x -> x + x) 2}";;
let ex5 = "Raise #Hello (1+1)";;
let ex6 = "Raise #Hello (Raise #Hello2 (1+1))";;
(* parse: expr = (Raise ("#Hello", (Raise ("#Hello2", (Plus ((Int 1), (Int 1)))))))
peu:  string = "Raise #Hello2 2" *)
let ex7 = "1+True";;
let ex8 = "(Function x -> Try (x=True) With #Return n -> n) 3";;
let ex9 = "Try (1=True) With #Return n -> n";;
let ex10 = "Try (1=1) With #Return n -> n";;
let ex11 = "Let x = {a=5} In x.b";;
let ex12 = "Try (Let x = {a=5} In x.b) With #Return n -> n";;
let ex13 = "{a = 1; b = 1} @ {c = 2; d = 1}";;

let ex14 = "{a = 1; b = 1} @ {a = 2; c = 1}";;
let ex15 = "{}";;


