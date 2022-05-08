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
exception NotTypable;;


(*
 * If you would like typechecking to be enabled by your interpreter by default,
 * then change the following value to true.  Whether or not typechecking is
 * enabled by default, you can explicitly enable it or disable it using
 * command-line arguments. 
 *) 
let typecheck_default_enabled = true;;

  let rec lookup_type list x = 
    match list with
    | (a, b)::tl -> if x = a then b else (lookup_type tl x)
    | [] -> raise NotTypable
    ;;

  let counter = ref 1;;


(* refer to: https://stackoverflow.com/a/30634435
   remove duplicate tuples in a list *)
let cons_uniq xs x = if List.mem x xs then xs else x :: xs;;

let remove_duplicates xs = List.rev (List.fold_left cons_uniq [] xs);;

 let rec type_generate e gamma_list carry =
    match e with
  | Int a -> (TInt, [])
  | Bool b -> (TBool, [])
  | Plus(e1, e2) -> 
    (
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let equation = (eq1 @ eq2 @ [(s1, TInt); (s2, TInt)]) in
      (TInt, equation)
    )
  | Minus(e1, e2) ->
    (
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let equation = (eq1 @ eq2 @ [(s1, TInt); (s2, TInt)]) in
      (TInt, equation)
    )
  | Equal(e1, e2) -> (
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let equation = (eq1 @ eq2 @ [(s1, TInt); (s2, TInt)]) in
      (TBool, equation)
    )
  | And(e1, e2) -> (
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let equation = (eq1 @ eq2 @ [(s1, TBool); (s2, TBool)]) in
      (TBool, equation)
    )
  | Or(e1, e2) -> (
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let equation = (eq1 @ eq2 @ [(s1, TBool); (s2, TBool)]) in
      (TBool, equation)
    )
  | Not(e1) -> (
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let equation = (eq1 @ [(s1, TBool)]) in
      (TBool, equation)
    )
  | If(e1, e2, e3) -> (
      (* let cnt = ((help_cnt e) + carry) in *)
      let cnt = (!counter) in incr counter;
      let alpha = TVar("a" ^ string_of_int(cnt)) in
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let (s3, eq3) = (type_generate e3 gamma_list 1) in
      let equation = (eq1 @ eq2 @ eq3 @ [(s1, TBool); (s2, alpha); (s3, alpha); (s2, s3)]) in
      (alpha, equation)
    )
  | Var(x) -> (
      (* look up the type of variable x in gamma_list *)
      let tau = ((lookup_type gamma_list x)) in
      (tau, [])
    )
  | Function(x, e1) -> (
      (* 1. append x: tick_a into gamma list
      2. tick_a = type_generate x and tau, equation = type_generate e
      3. return TArrow(tick_a, tau) @ equation *)
      (* let cnt = ((help_cnt e) + carry) in *)
      let cnt = (!counter) in incr counter;
      let tick_a = TVar("a" ^ string_of_int(cnt)) in
      let (tau, equation) = (type_generate e1 ((x, tick_a)::gamma_list) 0) in
      (TArrow(tick_a, tau), equation)
    )
  | Appl(e1, e2) -> (
      (* let cnt = ((help_cnt e) + carry) in *)
      let cnt = (!counter) in incr counter;
      let alpha = TVar("a" ^ string_of_int(cnt)) in
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0) in
      let equation = (eq1 @ eq2 @ [(s1, TArrow(s2, alpha))]) in
      (alpha, equation)
    )
  | Let(x, e1, e2) -> 
    (
      type_generate (Appl (Function(x, e2), e1)) gamma_list 0
    )
  | _ -> raise NotTypable;;


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
    );;
let rec add_closure equation =  
  (* for each of the equation tau0 -> tau0' = tau1 -> tau1' in equation, add tau0 = tau1 and tau0' = tau1' into equation *)
  (* For each set of equations (τ0, τ1) and (τ1, τ2) in E,add the equation τ0 =τ2 to E (by transitivity). *)
  match equation with
  | [] -> []
  | (TArrow(tau0, tau0'), TArrow(tau1, tau1'))::tl -> 
    (
      [(tau0, tau1); (tau0', tau1')] @ (add_closure tl)
    )
    (* nested for loop to check every 2 tuples in list *)
  | hd::tl ->
    (
      (help_check hd tl) @ (add_closure tl) 
    );;

let rec perform_closure equation = 
  (
    let new_equation = (remove_duplicates (equation @ (add_closure equation))) in
      if ((List.length new_equation) = (List.length equation)) 
        then equation else (perform_closure new_equation)
  );;


(* helper *)
let rec check_contain tau tick_b = 
  (* check if tau contains tick_b *)
  match tau with
  | TVar(x) -> if TVar(x) = tick_b then true else false
  | TArrow(tau1, tau2) -> (check_contain tau1 tick_b) || (check_contain tau2 tick_b)
  | _ -> false;;

let rec check_valid tau1 tau2 = 
  match (tau1, tau2) with
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | (TArrow(tau3, tau4), TArrow(tau5, tau6)) -> (check_valid tau3 tau5) && (check_valid tau4 tau6)
  | (TVar(x), _) -> true
  | (_, TVar(x)) -> true
  | _ -> false;;

(* let rec check_inconsistencies equation =
  (* check tuples in equation list e.g. (tau0, tau1) to see if tau0 = tau1 *)
    (* if inconsistencies are uncovered, such as Int = Bool, Bool = tau -> tau', or Int = tau -> tau'
      then raise NotTypable *)
  match equation with
  | [] -> true
  | (tau0, tau1)::tl -> (
    match tl with
    | [] -> (check_valid tau0 tau1)
    | (tau, tau')::tail ->
      if (check_valid tau0 tau1)=false then false
      else if (tau0=tau) 
        then (if (check_valid tau1 tau') then (check_inconsistencies tl) else false)
      else if (tau0=tau') 
        then (if (check_valid tau1 tau) then (check_inconsistencies tl) else false)
      else if (tau1=tau) 
        then (if (check_valid tau0 tau') then (check_inconsistencies tl) else false)
      else if (tau1=tau') 
        then (if (check_valid tau0 tau) then (check_inconsistencies tl) else false)
      (* check self-referential: raise error if there is (tick_a, tau) in equation and tau1 occurs in tau *)
      (* else if ((tau = tau0) && (check_contain tau tau1)) then false 
      else if ((tau = tau1) && (check_contain tau tau0)) then false
      else if ((tau' = tau0) && (check_contain tau' tau1)) then false
      else if ((tau' = tau1) && (check_contain tau' tau0)) then false *)
      else (check_inconsistencies tl)
  );; *)

let rec inner_looper hd tail = 
  (* loop through tail. if hd != tail then false else true;; *)
  match tail with
  | [] -> (let (tau0, tau1) = hd in (check_valid tau0 tau1))
  | (tau, tau')::tl ->
    (
      let (tau0, tau1) = hd in
      (
        if (check_valid tau0 tau1)=false then false
        else if (tau0=tau) 
          then (if (check_valid tau1 tau') then (inner_looper hd tl) else false)
        else if (tau0=tau') 
          then (if (check_valid tau1 tau) then (inner_looper hd tl) else false)
        else if (tau1=tau) 
          then (if (check_valid tau0 tau') then (inner_looper hd tl) else false)
        else if (tau1=tau') 
          then (if (check_valid tau0 tau) then (inner_looper hd tl) else false)
        else (inner_looper hd tl)
      )
    );;


let rec check_inconsistencies equation = 
  match equation with
  | [] -> true
  | (tau0, tau1)::tl -> (
      if (inner_looper (tau0, tau1) tl) then (check_inconsistencies tl) else false
    )
  ;;

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

let rec replace equation t v_string = 
  match equation with
  | [] -> t
  | (t1, t2)::tail ->
    if t1 = t then 
      (match t2 with
        | TVar v -> (
          if (v > v_string) then t2
          else (replace tail t v_string)
          )
        | _ -> t2)
    else if t2 = t then 
      (match t1 with
        | TVar v -> (
          if (v > v_string) then t1
          else (replace tail t v_string)
          )
        | _ -> t1)
    else (replace tail t v_string)
  ;;


let rec substitute_equation equation t = 
  match t with
    | TInt -> TInt
    | TBool -> TBool
    | TArrow(d, r) -> TArrow((substitute_equation equation d), (substitute_equation equation r))
    | TVar v -> 
      (let replace_type = (replace equation t v) in
      match replace_type with
      | TVar v1 ->
        if (v1 = v) then replace_type
        else (substitute_equation equation replace_type)  
      | _ -> (substitute_equation equation replace_type))
    ;;


let typecheck e = 
  counter := 1;
  let (tau, equation) = (type_generate e [] 0) in 
  let equation2 = (remove_duplicates (perform_closure equation)) in
  if (check_inconsistencies equation2) then (substitute_equation equation2 tau)
  else raise NotTypable;;