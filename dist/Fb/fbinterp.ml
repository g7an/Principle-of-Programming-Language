open Fbast

exception Bug;;

let rec subst (v: expr) (x) (e: expr) : expr = 
  match v with
  | Var y -> failwith "cannot substitue with a variable"
  | _ -> 
  (
  let rec substitute (e: expr) : expr = 
    match e with
    | Int x -> e
    | Bool x -> e
    | Var y -> if x = y then v else (Var (y))
    | Plus(e1, e2) -> (
      (* print_string "Plus subst \n"; *)
      Plus(substitute e1, substitute e2)
    )
    | Minus(e1, e2) -> Minus(substitute e1, substitute e2)
    | Equal(e1, e2) -> Equal(substitute e1, substitute e2)
    | And(e1, e2) -> And(substitute e1, substitute e2)
    | Or(e1, e2) -> Or(substitute e1, substitute e2)
    | Not(e1) -> Not(substitute e1)
    | If(e1, e2, e3) -> If(substitute e1, substitute e2, substitute e3)
    | Function(y, e1) -> Function(y, if x = y then e1 else (substitute e1))
    | Appl(e1, e2) -> Appl(substitute e1, substitute e2)
    | Let(y, e1, e2) ->  Let(y, substitute e1, if x = y then e2 else (substitute e2))
    | _ -> failwith "substitute error" 
  in substitute e
)

let rec check_closed exp list = 
  match exp with
  | Int x -> true
  | Bool x -> true
  | Var x -> (List.mem x list)
  | Plus(e1, e2) -> (check_closed e1 list && check_closed e2 list)
  | Minus(e1, e2) -> (check_closed e1 list && check_closed e2 list)
  | Equal(e1, e2) -> (check_closed e1 list && check_closed e2 list)
  | And(e1, e2) -> (check_closed e1 list && check_closed e2 list)
  | Or(e1, e2) -> (check_closed e1 list && check_closed e2 list)
  | Not(e1) -> (check_closed e1 list)
  | If(e1, e2, e3) -> (check_closed e1 list && check_closed e2 list && check_closed e3 list)
  | Function(x, e1) ->  
    (
      check_closed e1 (x::list)
    )
  | Appl(e1, e2) -> (check_closed e1 list && check_closed e2 list)
  | Let(x, e1, e2) -> 
    (
      check_closed e1 (x::list) && check_closed e2 (x::list)
    )
  | _ -> false

(*
 * Replace this with your interpreter code.
 *)
let rec eval e = 
  match e with
  | Int x -> (Int x)
  | Bool x -> (Bool x)
  | Var x -> failwith "Cannot have a variable in the evaluator"
  
(* int *)
  | Plus(e1, e2) -> 
    (
      (* print_string "Plus: \n"; *)
    let (v1, v2) = (eval e1, eval e2) in 
  (* interpreter for Plus 2 integers *)
        match (v1, v2) with
        |  (Int x, Int y) -> (Int (x + y) )
        | _ -> (failwith "plus")
    )

  | Minus(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
  (* interpreter for Minus 2 integers *)
        match (v1, v2) with
        (Int x, Int y) -> (Int (x - y))
        | _ -> (failwith "minus")
    )

  | Equal(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
  (* interpreter for Equal 2 integers *)
        match (v1, v2) with
        (Int x, Int y) -> (Bool (x = y))
        | _ -> (failwith "equal")
    )
  
(* bool *)
  | And(e1, e2) -> 
    (let (v1, v2) = (eval e1, eval e2) in
  (* interpreter for And 2 booleans *)
        match (v1, v2) with
        (Bool x, Bool y) -> (Bool (x && y))
        | _ -> (failwith "and")
    )
  | Or(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
  (* interpreter for Or 2 booleans *)
        match (v1, v2) with
        (Bool x, Bool y) -> (Bool (x || y))
        | _ -> (failwith "Or")
    )
  | Not(e1) ->
    (
      let v1 = (eval e1) in
  (* interpreter for Not 1 boolean *)
        match v1 with
        (Bool x) -> (Bool (not x))
        | _ -> (failwith "Not")
    )

  | If(e1, e2, e3) ->
    (let v1 = (eval e1) in
  (* interpreter for If 3 expr *)
        match v1 with
        (Bool x) -> if x then eval e2 else eval e3
        | _ -> (failwith "If")
    )
  | Function(x, e1) -> 
    (
     if (check_closed e []) then (Function (x, e1)) else failwith "expression not closed"
    )
  | Appl(e1, e2) ->
    (
    (* print_string "Appl: \n";   *)
    let v2 = (eval e2) in
  (* interpreter for Appl 2 expr *)
        match e1 with
        | (Function(x, y1)) -> (eval (subst v2 x y1))
        | Appl(e3, e4) -> 
            let v1 = (eval e1) in
            (* match v1 with *)
            (eval (Appl(v1, e2)))
            (* |_ -> (failwith "Appl") *)
        | _ -> 
          (failwith "Application rule")
    )
  | Let(x, e1, e2) -> 
    (
      (* print_string "Let: \n";
      let v1 = (eval e1) in 
      let e2' = subst v1 x e2 in
      eval e2'  *)
      (* print_string "Let: \n"; *)
      eval (Appl (Function(x, e2), e1))
    )
  | _ -> (failwith "not matched")

  

(* a function implementing variable substitution  *)
(* Want	to	replace	x		(and	only	x)	with	v *)
(* let subst x e' e  = "(Fun x -> ("^e'^")("^e^"))" ;; *)

(* x is a variable *)




(* let subst x e' e = "(Fun x -> ("^e'^")("^e^"))" ;; *)

(* a function which determines whether or not an expression is closed  *)
(* let check_closed e =  *)
  

