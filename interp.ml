(* This file is the only one you should modify for problems 2 and 3 *)
open Ast

exception InterpUnimplemented
exception DoNotTakeStepWithSkip

(* There are simpler heap definitions, but these will be easier
   for extending/modifying to support pushheap, popheap, pushvar, and popvar *)
let empty_heap = []
let empty_list = []
let rec get_var heap str =
  match heap with
      [] -> 0
    | (str',i, l)::tl -> if str=str' then i else get_var tl str
let rec set_var heap str num =
  match heap with
      [] -> [(str,num, [])]
    | (str',i, l)::tl -> if str=str'
                      then (str,num, l)::tl
                      else (str',i, l)::(set_var tl str num)

let rec push_var heap str =
  match heap with
    [] -> [(str, 0, [0])]
  | (str', i, l) :: tl ->
    if str = str'
    then (str, i, i::l)::tl
    else (str', i, l)::(push_var tl str)

let rec pop_var heap str =
  match heap with
    [] -> []
  | (str', i, l)::tl ->
    if str = str'
    then (
      match l with
        [] -> (str, i, l)::tl
      | hd::rest_list -> (str, hd, rest_list)::tl
      )
    else (str', i, l)::(pop_var tl str)


let rec interp_exp h e =
  let interp_exp_r = interp_exp h in
  match e with
      Int i -> i
    | Var v -> get_var h v
    | Plus(e1,e2)  -> (interp_exp_r e1) + (interp_exp_r e2)
    | Times(e1,e2) -> (interp_exp_r e1) * (interp_exp_r e2)

let rec interp_step l h s=
  match s with
      Skip ->  raise DoNotTakeStepWithSkip
    | Assign(v,e) -> (l, set_var h v (interp_exp h e), Skip)
    | Seq(Skip,s2) -> (l, h, s2)
    | Seq(s1,s2) ->
      let (l3, h3, s3) = interp_step l h s1 in
      (l3, h3, Seq(s3,s2))
    | If(e,s1,s2) ->
      if((interp_exp h e) <= 0)
      then (l, h, s2)
      else (l, h, s1)
    | While(e,s1) -> (l, h, If(e,Seq(s1,s),Skip))
    | Pushheap -> (h::l, h, Skip)
    | Popheap(x) ->
      (match l with
        [] -> (l, h, Skip)
      | hd::tail -> (tail, set_var hd x (get_var h x), Skip))
    | Pushvar(x) -> (l, push_var h x, Skip)
    | Popvar(x) -> (l, pop_var h x, Skip)

let rec iter l h s=
  match (h,s) with
      (_,Skip) -> get_var h "ans"
    | _ -> let (l', h', s') = interp_step l h s in iter l' h' s'

let interp = iter empty_list empty_heap
