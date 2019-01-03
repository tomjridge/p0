(** Monadic parser combinators *)


include String_util

include Types


(* naive monadic parsing -------------------------------------------- *)

(* experiment with monadic parsing; 'a m takes a string and returns an
   'a * string or an error/noparse indication *)

let bind (f:'a -> 'b m) (x:'a m) :'b m = 
  fun s -> x s |> function | None -> None | Some (v,s) -> f v s
let ( |>> ) x f = x |> bind f

let return x s = Some(x,s)

(** Parse one then another; return a pair *)
let then_ a b = a |>> fun x -> b |>> fun y -> return (x,y)

(** Infix for then_ *)
let ( -- ) = then_

(* FIXME improve this by using the result of the parse subsequently *)
(* let can x s = Some (x s <> None,s) *)

let a lit s = 
  if starts_with ~prefix:lit s 
  then drop (String.length lit) s |> fun s' -> Some(lit,s') 
  else None


(* let upto_a lit = Tjr_substring.upto_re ~re:Re.(literal lit) *)

(* FIXME following should use prefix *)
let upto_a lit = 
  assert(lit <> "");
  let len_lit = String.length lit in
  fun s -> 
    let len_s = String.length s in
    (* just search through s looking for lit *)
    let rec matches_at i i' = 
      (* assume i < len s *)
      match i' >= len_lit with
      | true -> true
      | _ -> 
        match i<len_s with
        | true -> 
          (String.get s i = String.get lit i') &&
          matches_at (i+1) (i'+1)
        | _ -> false
    in
    let rec f i = 
      if i+len_lit > len_s then None else
      if matches_at i 0 then Some i else f (i+1)
    in
    f 0 |> function
    | None -> None
    | Some i -> split_at s i |> fun (s1,s2) -> Some(s1,s2)
[@@warning "-w-40"]


(** Parse an optional something *)
let opt p s = 
  p s |> function
  | None -> Some(None,s) 
  | Some(x,s) -> Some(Some x,s)

(** Parse one or more *)
let rec plus ~sep p = 
  p |>> fun x ->
  (opt (sep -- plus ~sep p)) |>> function
  | None -> return [x]
  | Some (_,xs) -> return (x::xs)

(*
let save s = Some(s,s)

(* jump back in time *)
let restore s' s = Some((),s')
*)

(** Zero or more *)
let star ~sep p =
  opt p |>> function
  | None -> return []
  | Some x -> 
    opt (sep -- plus ~sep p) |>> function
    | None -> return [x]
    | Some (_,xs) -> return (x::xs)

(** Shortcut alternative *)
let alt a b = 
  opt a |>> function
  | None -> b
  | Some x -> return x

(** Infix alt *)
let ( || ) = alt       

(** Parse but return a unit *)
let discard p = p |>> fun _ -> return ()


(** Parse a then b, and discard both FIXME why pick this out? *)
let ( --- ) a b = discard (a -- b) 

let _Some x = Some x

(** Helper to avoid dependence on associativity of -- *)
let _3 ((x1,x2),x3) = (x1,x2,x3)



