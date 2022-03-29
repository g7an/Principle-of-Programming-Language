open Fbast


let rec subst (v: expr) (x) (e: expr) : expr = 
  let rec substitute (e: expr) : expr = 
    match e with
    | Int x -> e
    | Bool x -> e
    | Var y -> if x = y then v else (Var (y))
    | Plus(e1, e2) -> (
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
  | Var x -> failwith "Unbounded Variable Error"

(* int *)
  | Plus(e1, e2) -> 
    (
    let (v1, v2) = (eval e1, eval e2) in 
        match (v1, v2) with
        |  (Int x, Int y) -> (Int (x + y) )
        | _ -> (failwith "plus")
    )

  | Minus(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Int x, Int y) -> (Int (x - y))
        | _ -> (failwith "minus")
    )

  | Equal(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Int x, Int y) -> (Bool (x = y))
        | _ -> (failwith "equal")
    )
  
(* bool *)
  | And(e1, e2) -> 
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Bool x, Bool y) -> (Bool (x && y))
        | _ -> (failwith "and")
    )
  | Or(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Bool x, Bool y) -> (Bool (x || y))
        | _ -> (failwith "Or")
    )
  | Not(e1) -> 
    (
      let v1 = (eval e1) in
        match v1 with
        (Bool x) -> (Bool (not x))
        | _ -> (failwith "Not")
    )

  | If(e1, e2, e3) ->
    (let v1 = (eval e1) in
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
      match e1 with
      | Appl(e3, e4) -> 
        (eval (Appl(eval e1, e2)))
      | (Function(x, y1)) -> (eval (subst (eval e2) x y1))
      | _ -> 
        (failwith "Application rule")
    )
  | Let(x, e1, e2) -> 
    (
      eval (Appl (Function(x, e2), e1))
    )
  | _ -> (failwith "not matched")

  

