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

(*
 * Replace this with your typechecker code.  Your code should not throw the
 * following exception; if you need to raise an exception, create your own
 * exception type here.
 *) 

 (* 
 argument2: gamma_list;
 lookup the type of variable x 
 *)
 (* let rec lookup list x = 
  match list with
  | (a, b)::tl -> if x = a then b else (lookup tl x)
  | [] -> raise NotTypable
  ;; *)
(* find the second element of tuple by giving the first element of tuple, store all the possible results in a list *)
(* let rec lookup list x = 
  match list with
  | (a, b)::tl -> if x = a then b::(lookup tl x) else (lookup tl x)
  | [] -> []
  ;;

  (* find the smallest element in a list *)
let rec find_smallest list min_ele = 
  match list with
  | hd::tl -> if hd < min_ele then find_smallest tl hd else find_smallest tl min_ele
  | [] -> min_ele;;

let lookup_type list x = 
  (let lst = lookup list x in
  (
    (* if lst is empty raise not_typable error; else find the smallest element in lst *)
    if lst = [] then raise NotTypable
    else find_smallest lst (List.hd lst)
  ));; *)

  let rec lookup_type list x = 
    match list with
    | (a, b)::tl -> if x = a then b else (lookup_type tl x)
    | [] -> raise NotTypable
    ;;



let rec help_cnt e = 
  match e with
  | If(e1, e2, e3) -> 1 + (help_cnt e1) + (help_cnt e2) + (help_cnt e3)
  | Function(x, e2) -> 1 + (help_cnt e2)
  | Appl(e1, e2) -> 1 + (help_cnt e1) + (help_cnt e2)
  | _ -> 0
;;


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
      let cnt = ((help_cnt e) + carry) in
      let alpha = TVar("a" ^ string_of_int(cnt)) in
      let (s1, eq1) = (type_generate e1 gamma_list 0) in
      let (s2, eq2) = (type_generate e2 gamma_list 0)in
      let (s3, eq3) = (type_generate e3 gamma_list 1)in
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
      let cnt = ((help_cnt e) + carry) in
      let tick_a = TVar("a" ^ string_of_int(cnt)) in
      let (tau, equation) = (type_generate e1 ((x, tick_a)::gamma_list) 0) in
      (TArrow(tick_a, tau), equation)
    )
  | Appl(e1, e2) -> (
      let cnt = ((help_cnt e) + carry) in
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

(* 
let test = parse "If 1 = 2 Then (Fun x -> x) Else (Fun y -> y + 1)";;
-> 
  [(TInt, TInt); (TInt, TInt); ((TVar "a3"), TInt); (TInt, TInt);
  (TBool, TBool); ((TArrow ((TVar "a2"), (TVar "a2"))), (TVar "a3"));
  ((TArrow ((TVar "a3"), TInt)), (TVar "a3"))];;

  perform_closure 
  -> [(TInt, TInt); ((TVar "a3"), TInt); (TBool, TBool);
 ((TArrow ((TVar "a2"), (TVar "a2"))), (TVar "a3"));
 ((TArrow ((TVar "a3"), TInt)), (TVar "a3")); (TInt, (TVar "a3"));
 (TInt, (TArrow ((TVar "a2"), (TVar "a2"))));
 (TInt, (TArrow ((TVar "a3"), TInt)));
 ((TArrow ((TVar "a2"), (TVar "a2"))), (TArrow ((TVar "a3"), TInt)));
 ((TVar "a3"), (TArrow ((TVar "a2"), (TVar "a2"))));
 ((TVar "a3"), (TArrow ((TVar "a3"), TInt)));
 ((TArrow ((TVar "a2"), (TVar "a2"))), TInt);
 ((TArrow ((TVar "a3"), TInt)), TInt); ((TVar "a2"), (TVar "a3"));
 ((TVar "a2"), TInt); (TInt, (TVar "a2")); ((TVar "a3"), (TVar "a2"));
 ((TVar "a3"), (TVar "a3"));
 ((TArrow ((TVar "a2"), (TVar "a2"))), (TVar "a2"));
 ((TArrow ((TVar "a3"), TInt)), (TArrow ((TVar "a2"), (TVar "a2"))));
 ((TArrow ((TVar "a3"), TInt)), (TVar "a2"));
 ((TArrow ((TVar "a2"), (TVar "a2"))), (TArrow ((TVar "a2"), (TVar "a2"))));
 ((TArrow ((TVar "a3"), TInt)), (TArrow ((TVar "a3"), TInt)));
 ((TVar "a2"), (TArrow ((TVar "a3"), TInt)));
 ((TVar "a2"), (TArrow ((TVar "a2"), (TVar "a2"))));
 ((TVar "a2"), (TVar "a2"))]


let test = parse "Fun f -> Fun g -> If f 0 Then g f Else 0";;
f 0 : int -> bool
g f : bool -> int

a5 (int -> bool) -> a4 (bool -> int) -> a3 (int)

-> ((TArrow ((TVar "a5"), (TArrow ((TVar "a4"), (TVar "a3"))))),
 [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
  ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a2")))); ((TVar "a1"), TBool);
  ((TVar "a2"), (TVar "a3")); (TInt, (TVar "a3"))])

remove_duplicates (perform_closure [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
  ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a2")))); ((TVar "a1"), TBool);
  ((TVar "a2"), (TVar "a3")); (TInt, (TVar "a3"))]);;
-> [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
 ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a2")))); ((TVar "a1"), TBool);
 ((TVar "a2"), (TVar "a3")); (TInt, (TVar "a3")); ((TVar "a2"), TInt);
 ((TVar "a3"), TInt); ((TVar "a3"), (TVar "a2")); ((TVar "a3"), (TVar "a3"));
 (TInt, (TVar "a2")); (TInt, TInt)]

 substitute_equation (TArrow ((TVar "a5"), (TArrow ((TVar "a4"), (TVar "a3"))))) [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
 ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a2")))); ((TVar "a1"), TBool);
 ((TVar "a2"), (TVar "a3")); (TInt, (TVar "a3")); ((TVar "a2"), TInt);
 ((TVar "a3"), TInt); ((TVar "a3"), (TVar "a2")); ((TVar "a3"), (TVar "a3"));
 (TInt, (TVar "a2")); (TInt, TInt)];;

let test = parse "(Fun x -> x + ((Fun x -> If x Then 0 Else 1) True))";;
  (Function ((Ident "x"),
     (Plus ((Var (Ident "x")),
        (Appl (
           (Function ((Ident "x"), (If ((Var (Ident "x")), (Int 0), (Int 1))))),
           (Bool true)))
        ))
     )) 

     ((TArrow ((TVar "a1"), TInt)),
 [((TVar "a1"), TBool); (TInt, (TVar "a1")); (TInt, (TVar "a1"));
  ((TArrow ((TVar "a2"), (TVar "a1"))), (TArrow (TBool, (TVar "a3"))));
  ((TVar "a1"), TInt); ((TVar "a3"), TInt)])

let test = parse "(Fun x -> Fun x -> x)";; 
((TArrow ((TVar "a2"), (TArrow ((TVar "a1"), (TVar "a1"))))), [])
   perform_closure (TArrow ((TVar "tick_a1"), (TVar "tick_a1"))) [];;

   let test = parse "1+True";; 
   (TInt, [(TInt, TInt); (TBool, TInt)])
   
   perform_closure TInt [(TInt, TInt); (TBool, TInt)];;
   [(TInt, TBool)]

   let test = parse "If 1 = 2 Then 1 + 1 Else 1 - 1";; 
   ((TVar "tick_a1"),
 [(TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, TInt);
  (TInt, TInt); (TBool, TBool); (TInt, (TVar "tick_a1"));
  (TInt, (TVar "tick_a1"))])
  
  perform_closure (TVar "tick_a1") [(TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, TInt);
  (TInt, TInt); (TBool, TBool); (TInt, (TVar "tick_a1"));
  (TInt, (TVar "tick_a1"))]

  remove_duplicates [(TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, TInt);
 (TInt, (TVar "tick_a1")); (TInt, (TVar "tick_a1")); (TInt, TInt);
 (TInt, TInt); (TInt, TInt); (TInt, TInt); (TInt, (TVar "tick_a1"));
 (TInt, (TVar "tick_a1")); (TInt, TInt); (TInt, TInt); (TInt, TInt);
 (TInt, (TVar "tick_a1")); (TInt, (TVar "tick_a1")); (TInt, TInt);
 (TInt, TInt); (TInt, (TVar "tick_a1")); (TInt, (TVar "tick_a1"));
 (TInt, TInt); (TInt, (TVar "tick_a1")); (TInt, (TVar "tick_a1"));
 (TInt, (TVar "tick_a1")); (TInt, (TVar "tick_a1"));
 ((TVar "tick_a1"), (TVar "tick_a1"))];;

  check_inconsistencies [(TInt, TInt); (TInt, (TVar "tick_a1")); ((TVar "tick_a1"), (TVar "tick_a1"))];;

  let test = parse "(Fun x -> x = 1) True";;

  perform_closure [((TVar "tick_a1"), TInt); (TInt, TInt);
  ((TArrow ((TVar "tick_a1"), TBool)), (TArrow (TBool, (TVar "tick_a2"))))]
  -> [((TVar "tick_a1"), TInt); (TInt, TInt);
 ((TArrow ((TVar "tick_a1"), TBool)), (TArrow (TBool, (TVar "tick_a2"))));
 ((TVar "tick_a1"), TInt); ((TVar "tick_a1"), TBool);
 (TBool, (TVar "tick_a2"))]

 remove_duplicates [((TVar "tick_a1"), TInt); (TInt, TInt);
 ((TArrow ((TVar "tick_a1"), TBool)), (TArrow (TBool, (TVar "tick_a2"))));
 ((TVar "tick_a1"), TInt); ((TVar "tick_a1"), TBool);
 (TBool, (TVar "tick_a2"))]
 -> [((TVar "tick_a1"), TInt); (TInt, TInt);
 ((TArrow ((TVar "tick_a1"), TBool)), (TArrow (TBool, (TVar "tick_a2"))));
 ((TVar "tick_a1"), TBool); (TBool, (TVar "tick_a2"))]
check_inconsistencies [((TVar "tick_a1"), TInt); (TInt, TInt);
 ((TArrow ((TVar "tick_a1"), TBool)), (TArrow (TBool, (TVar "tick_a2"))));
 ((TVar "tick_a1"), TBool); (TBool, (TVar "tick_a2"))]

 let test = "Fun f -> f 100";;
 


*)
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

(* let rec perform_closure equation = equation @  
(if (add_closure equation) = [] then [] else (perform_closure (add_closure equation)));; *)
(* if the list is "stablized" (no more new tuples) then return the list, else check if new equation can be added *)
let rec perform_closure equation = 
  (
    let new_equation = (remove_duplicates (equation @ (add_closure equation))) in
      if ((List.length new_equation) = (List.length equation)) 
        then equation else (perform_closure new_equation)
  );;
  (* equation @  
(if (add_closure equation) = [] then [] else (perform_closure (add_closure equation)));; *)

(* 
let test = parse "(Fun s -> Fun x -> If s Then x Else x - 1)";; 

((TArrow ((TVar "a3"), (TArrow ((TVar "a2"), (TVar "a1"))))),
 [((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
  ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))])

perform_closure 
[((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
  ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))]

*)

    (* ocaml: remove duplicate tuples in list *)

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
   (* check_inconsistencies [((TVar "tick_a1"), TInt); 
 ((TVar "tick_a1"), TBool)];; *)
  (* check_inconsistencies [((TVar "tick_a1"), TInt); (TInt, TInt);
 ((TVar "tick_a1"), TBool); (TBool, (TVar "tick_a2"))];; *)
  (* check_inconsistencies [((TVar "tick_a1"), TInt); (TInt, TInt);
 ((TArrow ((TVar "tick_a1"), TBool)), (TArrow (TBool, (TVar "tick_a2"))));
 ((TVar "tick_a1"), TBool); (TBool, (TVar "tick_a2"))];; *)

  (* let rec check_cyclic equation *)

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
| TVar(x) -> 
  (
    match equation with
    | [] -> tau
    | hd::tl -> (
      (* if tau = tau0 then tau1 else if tau = tau1 then tau0 else (substitute_equation tau tl) *)
      let result = (find_substitution tau equation) in 
      (substitute_equation result tl)
    )  
  )
;;

(* 
let test = parse "Fun f -> Fun g -> If f 0 Then g f Else 0";;
f 0 : int -> bool
g f : bool -> int

a5 (int -> bool) -> a4 (bool -> int) -> a3 (int)

-> ((TArrow ((TVar "a5"), (TArrow ((TVar "a4"), (TVar "a3"))))),
 [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
  ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a1")))); ((TVar "a1"), TBool);
  ((TVar "a1"), (TVar "a3")); (TInt, (TVar "a3"))])

remove_duplicates (perform_closure [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
  ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a1")))); ((TVar "a1"), TBool);
  ((TVar "a1"), (TVar "a3")); (TInt, (TVar "a3"))]) ;;
-> [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
 ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a1")))); ((TVar "a1"), TBool);
 ((TVar "a1"), (TVar "a3")); (TInt, (TVar "a3")); (TBool, (TVar "a3"));
 ((TVar "a1"), TInt); (TBool, TInt); ((TVar "a3"), TInt); (TInt, TBool);
 ((TVar "a3"), (TVar "a1")); ((TVar "a3"), (TVar "a3")); ((TVar "a3"), TBool);
 (TInt, (TVar "a1")); (TBool, (TVar "a1")); (TInt, TInt); (TBool, TBool)]

 check_inconsistencies [((TVar "a5"), (TArrow (TInt, (TVar "a1"))));
 ((TVar "a4"), (TArrow ((TVar "a5"), (TVar "a1")))); ((TVar "a1"), TBool);
 ((TVar "a1"), (TVar "a3")); (TInt, (TVar "a3")); (TBool, (TVar "a3"));
 ((TVar "a1"), TInt); (TBool, TInt); ((TVar "a3"), TInt); (TInt, TBool);
 ((TVar "a3"), (TVar "a1")); ((TVar "a3"), (TVar "a3")); ((TVar "a3"), TBool);
 (TInt, (TVar "a1")); (TBool, (TVar "a1")); (TInt, TInt); (TBool, TBool)];;

substitute_equation (TArrow ((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a2"))))) 
  [((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a1"))));
    ((TVar "a4"), (TArrow ((TVar "a1"), (TVar "a2"))));
    ((TArrow ((TVar "a3"), (TVar "a1"))), (TArrow ((TVar "a1"), (TVar "a2"))))]
  ;;
  -> (TArrow ((TArrow ((TVar "a3"), (TVar "a1"))),
   (TArrow ((TVar "a3"), (TVar "a2")))))


substitute_equation 
(TArrow ((TVar "a3"), (TArrow ((TVar "a2"), (TVar "a1"))))) 
[((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))] 
*)
(* substitute_equation (TArrow ((TVar "tick_a1"), (TVar "tick_a1"))) [];; *)

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
  let (tau, equation) = (type_generate e [] 0) in 
  let equation2 = (remove_duplicates (perform_closure equation)) in
  if (check_inconsistencies equation2) then (substitute_equation tau equation2)
  else raise NotTypable;;

(* 
let test = parse "(Fun x -> x + ((Fun x -> If x Then 0 Else 1) True))";;
let test = parse "Fun x -> x + ((Fun x -> If x Then 0 Else 1) True)";;
 type_generate test [];;
 -> ((TArrow ((TVar "a1"), TInt)),
 [((TVar "a1"), TBool); (TInt, (TVar "a1")); (TInt, (TVar "a1"));
  ((TArrow ((TVar "a2"), (TVar "a1"))), (TArrow (TBool, (TVar "a3"))));
  ((TVar "a1"), TInt); ((TVar "a3"), TInt)])

  perform_closure [((TVar "a1"), TBool); (TInt, (TVar "a1")); (TInt, (TVar "a1"));
  ((TArrow ((TVar "a2"), (TVar "a1"))), (TArrow (TBool, (TVar "a3"))));
  ((TVar "a1"), TInt); ((TVar "a3"), TInt)];;
  -> 
 [((TVar "a1"), TBool); (TInt, (TVar "a1"));
 ((TArrow ((TVar "a2"), (TVar "a1"))), (TArrow (TBool, (TVar "a3"))));
 ((TVar "a1"), TInt); ((TVar "a3"), TInt); (TBool, TInt);
 ((TVar "a1"), (TVar "a1")); ((TVar "a1"), (TVar "a3")); ((TVar "a2"), TBool);
 (TBool, (TVar "a1")); (TBool, (TVar "a3")); ((TVar "a1"), (TVar "a2"));
 (TInt, (TVar "a3")); ((TVar "a3"), TBool); (TInt, (TVar "a2"));
 (TBool, TBool); (TBool, (TVar "a2")); (TInt, TBool); (TInt, TInt);
 ((TVar "a3"), (TVar "a2")); ((TVar "a2"), (TVar "a1"));
 ((TVar "a2"), (TVar "a3")); ((TVar "a3"), (TVar "a3")); ((TVar "a2"), TInt);
 ((TVar "a2"), (TVar "a2")); ((TVar "a3"), (TVar "a1"))]

  check_inconsistencies [((TVar "a1"), TBool); (TInt, (TVar "a1"));
 ((TArrow ((TVar "a2"), (TVar "a1"))), (TArrow (TBool, (TVar "a3"))));
 ((TVar "a1"), TInt); ((TVar "a3"), TInt); (TBool, TInt);
 ((TVar "a1"), (TVar "a1")); ((TVar "a1"), (TVar "a3")); ((TVar "a2"), TBool);
 (TBool, (TVar "a1")); (TBool, (TVar "a3")); ((TVar "a1"), (TVar "a2"));
 (TInt, (TVar "a3")); ((TVar "a3"), TBool); (TInt, (TVar "a2"));
 (TBool, TBool); (TBool, (TVar "a2")); (TInt, TBool); (TInt, TInt);
 ((TVar "a3"), (TVar "a2")); ((TVar "a2"), (TVar "a1"));
 ((TVar "a2"), (TVar "a3")); ((TVar "a3"), (TVar "a3")); ((TVar "a2"), TInt);
 ((TVar "a2"), (TVar "a2")); ((TVar "a3"), (TVar "a1"))];;


let test = parse "Fun f -> Fun x -> f (f x)";; 
-> (
  (TArrow ((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a2"))))),
 [((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a1"))));
  ((TVar "a4"), (TArrow ((TVar "a1"), (TVar "a2"))))]
  )

  remove_duplicates (perform_closure [((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a1"))));
  ((TVar "a4"), (TArrow ((TVar "a1"), (TVar "a2"))))] );;
-> [((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a1"))));
    ((TVar "a4"), (TArrow ((TVar "a1"), (TVar "a2"))));
    ((TArrow ((TVar "a3"), (TVar "a1"))), (TArrow ((TVar "a1"), (TVar "a2"))))]
   
  substitute_equation (TArrow ((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a2"))))) 
  [((TVar "a4"), (TArrow ((TVar "a3"), (TVar "a1"))));
    ((TVar "a4"), (TArrow ((TVar "a1"), (TVar "a2"))));
    ((TArrow ((TVar "a3"), (TVar "a1"))), (TArrow ((TVar "a1"), (TVar "a2"))))]
  ;;
*)

  (* let test = parse "(Fun s -> Fun x -> If s Then x Else x - 1)";;
   -> (
     (TArrow ((TVar "a3"), (TArrow ((TVar "a2"), (TVar "a1"))))),
   [((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
  ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))]
  )
  remove_duplicates (perform_closure [((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
  ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))]);;
  -> [((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
 ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))]

 check_inconsistencies [((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
 ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))] ;;

 substitute_equation (TArrow ((TVar "a3"), (TArrow ((TVar "a2"), (TVar "a1"))))) [((TVar "a2"), TInt); (TInt, TInt); ((TVar "a3"), TBool);
 ((TVar "a2"), (TVar "a1")); (TInt, (TVar "a1"))]


*)
(* let test = parse "0 0";; *)
(* let test = parse "Fun x -> Fun y -> Fun z -> If True Then (If True Then x Else y) Else (If True Then y Else z)";; *)
(* let test = parse "Fun x -> Fun x -> x";; *)
(* 
let test = parse "(0 0)";;

perform_closure [(TInt, (TArrow (TInt, (TVar "tick_a1"))))]

remove_duplicates [(TInt, (TArrow (TInt, (TVar "tick_a1"))))]

check_inconsistencies [(TInt, (TArrow (TInt, (TVar "tick_a1"))))]

let test = parse "Fun x -> Fun x -> x";;

type: TArrow ((TVar "tick_a2"), (TArrow ((TVar "tick_a1"), (TVar "tick_a2")))))

perform_closure []

check_inconsistencies 

*)


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