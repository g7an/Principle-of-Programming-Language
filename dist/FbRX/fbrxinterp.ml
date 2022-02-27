open Fbrxast

exception NotClosed

let fbLabelNotFound = Raise("#LabelNotFound", Int(0))
let fbTypeMismatch = Raise("#TypeMismatch", Int(0))


(*
 * Replace this with your interpreter code.
 *)
let eval e = e
