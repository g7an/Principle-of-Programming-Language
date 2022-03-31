open Fbrxast

exception NotClosed

let fbLabelNotFound = Raise("#LabelNotFound", Int(0))
let fbTypeMismatch = Raise("#TypeMismatch", Int(0))


(*
 * Replace this with your interpreter code.
 *)


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
    | Let(y, e1, e2) ->   Let(y, substitute e1, if x = y then e2 else (substitute e2))
    | Record(e) -> Record(e)
    | Select(l, e) -> Select(l, substitute e)
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

let rec lookupRecord body (Lab l) = match body with
  [] -> (fbLabelNotFound)
| (Lab l1, v)::t -> if l = l1 then v else lookupRecord t (Lab l);;

let rec containsLabel body (Lab l) = match body with
  [] -> false
| (Lab l1, v)::t -> if l = l1 then true else containsLabel t (Lab l);;

(* Append records without duplicates *)
let rec appendRecords list1 list2 = 
  match list1 with
  | [] -> list2
  | (Lab l1, v)::t -> if (containsLabel list2 (Lab l1)) then appendRecords t list2 else ((Lab l1, v)::(appendRecords t list2));;
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
        | _ -> (fbTypeMismatch)
    )

  | Minus(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Int x, Int y) -> (Int (x - y))
        | _ -> (fbTypeMismatch)
    )

  | Equal(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Int x, Int y) -> (Bool (x = y))
        | _ -> (fbTypeMismatch)
    )
  
(* bool *)
  | And(e1, e2) -> 
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Bool x, Bool y) -> (Bool (x && y))
        | _ -> (fbTypeMismatch)
    )
  | Or(e1, e2) ->
    (let (v1, v2) = (eval e1, eval e2) in
        match (v1, v2) with
        (Bool x, Bool y) -> (Bool (x || y))
        | _ -> (fbTypeMismatch)
    )
  | Not(e1) -> 
    (
      let v1 = (eval e1) in
        match v1 with
        (Bool x) -> (Bool (not x))
        | _ -> (fbTypeMismatch)
    )

  | If(e1, e2, e3) ->
    (let v1 = (eval e1) in
        match v1 with
        (Bool x) -> if x then eval e2 else eval e3
        | _ -> (fbTypeMismatch)
    )
  | Function(x, e1) -> 
    (
     if (check_closed e []) then (Function (x, e1)) else raise NotClosed
    )
  | Appl(e1, e2) ->
    (
      match e1 with
      | Appl(e3, e4) -> 
        (eval (Appl(eval e1, e2)))
      | (Function(x, y1)) -> (eval (subst (eval e2) x y1))
      | _ -> (fbTypeMismatch)
    )
  | Let(x, e1, e2) -> 
    (
      eval (Appl (Function(x, e2), e1))
    ) 
    (* records *)
  | Record(body) -> Record(evalRecord body)
  | Select(l, e) -> 
    (match eval e with
      Record(body) -> lookupRecord body l
      | _ -> (fbTypeMismatch) 
    )
  | Append(e1, e2) -> (
    match (eval e1) with
     Record(body1) -> 
      (
        match (eval e2) with
          Record(body2) -> Record(appendRecords body1 body2)
        | _ -> (fbTypeMismatch)
      )
    | _ -> (fbTypeMismatch) 
  )
    (* exceptions *)
  | Raise(exnid, e) -> (
      match e with
      | Raise(exnid1, e1) -> Raise(exnid1, (eval e1))
      | _ -> (eval e)
    )
  | Try(e1, exnid, var, e2) -> (
      let v1 = (eval e1) in
        match v1 with
        | Raise(exnid1, e3) -> (subst (v1) var (e2))
        | _ -> (eval e1)
    )
  | _ -> (fbTypeMismatch)
and evalRecord body = match body with
  [] -> []
| (Lab l, e)::t -> (Lab l, eval e)::evalRecord t ;;

  


(* let eval e = e *)
