(* 
open Fbdk;;
open Ast;;
open Typechecker;;
open Pp;;
open Debugutils;; 
let tc  s = show_fbtype @@ typecheck @@ parse s;;
*)

open Fbast;;

exception TypecheckerNotImplementedException;;
exception ListElementNotFoundException;;
exception TypeError;;


(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = true;;

(*
 * Replace this with your typechecker code.  Your code should not throw the
 * following exception; if you need to raise an exception, create your own
 * exception type here.
 *) 

 (* 
 argument2: gamma_list;
 lookup the type of variable x 
 *)
(* find the second element of tuple by giving the first element of tuple *)
let rec lookup list x = 
  match list with
  | (a, b)::tl -> if x = a then b else (lookup tl x)
  | [] -> raise ListElementNotFoundException
  ;;

let rec help_cnt e = 
  match e with
  | If(e1, e2, e3) -> 1 + (help_cnt e1) + (help_cnt e2) + (help_cnt e3)
  | Function(x, e2) -> 1 + (help_cnt e2)
  | Appl(e1, e2) -> 1 + (help_cnt e1) + (help_cnt e2)
  | _ -> 0
;;

(* helper *)
let rec help_check hd tl = 
  (* loop through tail and add the qualified new tuple to the return list *)
  match tl with
  | [] -> []
  | (tau2, tau3)::tail -> 
    (
      let (tau0, tau1) = hd in
      (
          if tau0 = tau2 
              then ([(tau1,tau3)] @ (help_check hd tail)) 
          else if tau0 = tau3 
              then ([(tau1,tau2)] @ (help_check hd tail)) 
          else if tau1 = tau2 
              then ([(tau0,tau3)] @ (help_check hd tail)) 
          else if tau1 = tau3 
              then ([(tau0,tau2)] @ (help_check hd tail)) 
          else (help_check hd tail)
      )
    )



 let rec type_generate e gamma_list =
    match e with
  | Int a -> (TInt, [])
  | Bool b -> (TBool, [])
  | Plus(e1, e2) -> 
    (
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TInt); (s2, TInt)]) in
      (TInt, equation)
    )
  | Minus(e1, e2) ->
    (
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TInt); (s2, TInt)]) in
      (TInt, equation)
    )
  | Equal(e1, e2) -> (
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TInt); (s2, TInt)]) in
      (TBool, equation)
    )
  | And(e1, e2) -> (
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TBool); (s2, TBool)]) in
      (TBool, equation)
    )
  | Or(e1, e2) -> (
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TBool); (s2, TBool)]) in
      (TBool, equation)
    )
  | Not(e1) -> (
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let equation = (eq1 @ [(s1, TBool)]) in
      (TBool, equation)
    )
  | If(e1, e2, e3) -> (
      let cnt = (help_cnt e) in
      let alpha = TVar("tick_a" ^ string_of_int(cnt)) in
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let (s3, eq3) = (type_generate e3 gamma_list) in
      let equation = (eq1 @ eq2 @ eq3 @ [(s1, TBool); (s2, alpha); (s3, alpha)]) in
      (alpha, equation)
    )
  | Var(x) -> (
      (* look up the type of variable x in gamma_list *)
      let tau = (lookup gamma_list x) in
      (tau, [])
    )
  | Function(x, e1) -> (
      (* 1. append x: tick_a into gamma list
      2. tick_a = type_generate x and tau, equation = type_generate e
      3. return TArrow(tick_a, tau) @ equation *)
      let cnt = (help_cnt e) in
      let tick_a = TVar("tick_a" ^ string_of_int(cnt)) in
      let (tau, equation) = (type_generate e1 (gamma_list @ [(x, tick_a)])) in
      (TArrow(tick_a, tau), equation)
    )
  | Appl(e1, e2) -> (
      let cnt = (help_cnt e) in
      let alpha = TVar("tick_a" ^ string_of_int(cnt)) in
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TArrow(s2, alpha))]) in
      (alpha, equation)
    )
  | Let(x, e1, e2) -> 
    (
      type_generate (Appl (Function(x, e2), e1)) gamma_list
    )
  | _ -> failwith "Input not valid";;

(* let test = parse "(Fun x -> x)";; *)

let rec perform_closure tau equation =  
  (* for each of the equation tau0 -> tau0' = tau1 -> tau1' in equation, add tau0 = tau1 and tau0' = tau1' into equation *)
  (* For each set of equations (τ0, τ1) and (τ1, τ2) in E,add the equation τ0 =τ2 to E (by transitivity). *)
  match equation with
  | [] -> equation
  | (TArrow(tau0, tau0'), TArrow(tau1, tau1'))::tl -> 
    (
      [(tau0, tau1); (tau0', tau1')] @ (perform_closure tau tl)
    )
    (* nested for loop to check every 2 tuples in list *)
  | (tau0, tau1)::tl ->
    (
      let hd = (tau0, tau1) in 
      (help_check hd tl) @ (perform_closure tau tl)
    );;

(* helper *)
let rec check_contain tau tick_b = 
  (* check if tau contains tick_b *)
  match tau with
  | TVar(x) -> if TVar(x) = tick_b then true else false
  | TArrow(tau1, tau2) -> (check_contain tau1 tick_b) || (check_contain tau2 tick_b)
  | _ -> false;;

let rec check_inconsistencies equation =
  (* check tuples in equation list e.g. (tau0, tau1) to see if tau0 = tau1 *)
    (* if inconsistencies are uncovered, such as Int = Bool, Bool = tau -> tau', or Int = tau -> tau'
      then raise typeerror *)
  match equation with
  | [] -> true
  | (tau0, tau1)::tl -> (
    match tl with
    | [] -> true
    | (tau, tau')::tail ->
      if (tau0 <> tau1) then false
      (* check self-referential: raise error if there is (tick_a, tau) in equation and tau1 occurs in tau *)
      (* else if ((tau = tau0) && (check_contain tau tau1)) then false 
      else if ((tau = tau1) && (check_contain tau tau0)) then false
      else if ((tau' = tau0) && (check_contain tau' tau1)) then false
      else if ((tau' = tau1) && (check_contain tau' tau0)) then false *)
      else (check_inconsistencies tl)
  );;

  (* find_substitution, once find one, return *)
let rec find_substitution tau equation = 
match equation with
  | [] -> tau
  | (tau0, tau1)::tl -> 
  (
    if tau = tau0 then tau1
    else if tau = tau1 then tau0
    else (find_substitution tau tl)
   )
;;


let rec substitute_equation tau equation = 
match tau with
| TInt -> TInt
| TBool -> TBool
| TArrow(tau1, tau2) -> TArrow(substitute_equation tau1 equation, substitute_equation tau2 equation)
| TVar(x) -> let x = (find_substitution tau equation) in (substitute_equation x equation)
;;

(* nested for loop to loop through every 2 elements in a list *)

(* let w = parse "(1+2)";;
let func = parse "(Fun x -> x) 5";; 
let if_test = parse "If True Then 1 Else 2";;
let if_test2 = parse "If True Then (If True Then 3 Else 4) Else 2";;

*)
(* let func = parse "(Let x = 5 In Fun y -> x + y)";; *)

  (* "type_generate (1+2)";; *)
  (* type_generate @@ parse "If True Then 1 Else 2";; *)


let typecheck e = 
  let (tau, equation) = (type_generate e []) in 
  let equation2 = perform_closure tau equation in
  if (check_inconsistencies equation2) then (substitute_equation tau equation2)
  else raise TypeError;;


  (* | Var x -> (match x with 
            | y -> (TVar y, [])) *)
  
    (* TInt; [(tau, Int); (tau', Int)] *)
  (* | Var(x) -> (
      (* look up the type of variable x in gamma_list *)
      let tau = (lookup gamma_list x) in
      (tau, [])
    )
  | Function(x, e) -> (
      (* 1. append x: tick_a into gamma list
      2. tick_a = type_generate x and tau, equation = type_generate e
      3. return TArrow(tick_a, tau) @ equation *)
      let cnt = (help_cnt e) in
      let tick_a = TVar("tick_a" ^ string_of_int(cnt)) in
      let (tau, equation) = (type_generate e (gamma_list @ [(x, tick_a)])) in
      (TArrow(tick_a, tau), equation)
    )
  | Appl(e1, e2) -> (
      let cnt = (help_cnt e) in
      let (s1, eq1) = (type_generate e1 gamma_list) in
      let (s2, eq2) = (type_generate e2 gamma_list) in
      let equation = (eq1 @ eq2 @ [(s1, TArrow(s2, TVar("tick_a" ^ string_of_int(cnt))))]) in
      (s1, equation)
    ) *)