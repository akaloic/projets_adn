open Regex_base

type 'a expr =
  | Eps
  | Base of 'a
  | Joker
  | Concat of 'a expr * 'a expr 
  | Alt of 'a expr * 'a expr
  | Star of 'a expr

let rec repeat n l =
  if n = 0 then 
    []
  else 
    l @ repeat (n-1) l

let rec expr_repeat n e =
  if n = 0 then 
    Eps
  else 
    Concat(e, expr_repeat (n-1) e)

let is_empty e =
  match e with 
  | Eps -> true
  | Base x -> false
  | Joker -> false
  | Concat (g, d) -> is_empty g && is_empty d
  | Alt (g, d) -> is_empty g || is_empty d
  | Star _ -> true

let null e =
  is_empty e

let rec is_finite e =
  failwith "À compléter"

let product l1 l2 =
  failwith "À compléter"

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
