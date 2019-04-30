(** Str impl of re_ops *)

open Types
open String_util

type re = Str.regexp

(* re_ops ----------------------------------------------------------- *)

let literal s = Str.regexp_string s

let string_match ~re ~off s = 
  if (Str.string_match re s off) 
  then Some(Str.match_end ())  (* FIXME return all results? *)
  else None

let search_forward ~re ~off s =
  try 
    Str.search_forward re s off |> fun k ->
    Some k
  with Not_found -> None    


let re_ops = { literal; string_match; search_forward }


(* re and upto_re --------------------------------------------------- *)


let re ~re s = 
  match string_match ~re ~off:0 s with
  | None -> None
  | Some i -> Some(split_at s i)  (* FIXME return all results? *)

(** NOTE this requires that re is actually found *)
let upto_re ~re s =
  search_forward ~re ~off:0 s |> function
  | None -> None
  | Some i -> Some(split_at s i)


let re_parsers = { re; upto_re }
















(* old ================================================================== *)

(* regexp support --------------------------------------------------- *)

(*


let re ~re s = 
  match string_match ~re ~off:s.i_ s.s_ with
  | None -> []
  | Some i -> [i]  (* FIXME return all results? *)


let upto_re ~re s =
  search_forward ~re ~off:s.i_ s.s_ |> function
  | None -> []
  | Some k -> [k]


(** Parse a regular exp *)
let re re' = 
  fun s ->
  Tjr_substring.(re ~re:re' {s_=s;i_=0}) |> fun xs ->
  if xs <> []
  then split_at s (List.hd xs) |> fun (s1,s2) -> Some(s1,s2)
  else None [@@warning "-w-40"]

  (** Parse upto a regular exp *)
  let upto_re re' = 
    fun s ->
      Tjr_substring.(upto_re ~re:re' {s_=s;i_=0}) |> fun xs ->
      if xs <> []
      then split_at s (List.hd xs) |> fun (s1,s2) -> Some(s1,s2)
      else 
        (* not found anywhere, so consume the whole string *)
        Some(s,"") [@@warning "-w-40"]
*)
