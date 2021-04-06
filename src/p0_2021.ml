(** Monadic parser combinators.

Return at most 1 parse at each point (so, alternatives are ordered,
   and the first applicable is taken).

A rewrite of 2017 version. *)


(* from Tjr_lib_core.Iter *)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x

type state = {
  init_input : string;
  len        : int; (* of init_input *)
  i          : int;
  debug      : bool;
  max        : int ref;  (* debug: this is the max i reached; updated in bind *)
}

let update_max s = s.max := max !(s.max) s.i; s

let string_to_state ~debug s = {
  init_input = s;
  len        = String.length s;
  i          = 0;
  debug;
  max        = ref 0;
}

module M : sig
  type 'a m 
  val return    : 'a -> 'a m
  val ( >>= )   : 'a m -> ('a -> 'b m) -> 'b m
  val run       : 'a m -> state -> ('a * state) option
  val inject    : (state -> ('a * state)option) -> 'a m
  val fail      : unit -> 'a m
end = struct
  type 'a m = state -> ('a * state)option

  let return (x:'a) : 'a m = fun s -> Some(x,s)

  let bind a ab = 
    fun t -> a t |> function
      | None -> None
      | Some (a,t) ->
        ignore(update_max t);
        ab a t 
  let ( >>= ) = bind

  let run x s = x s
           
  let inject f = f
  let fail () = fun _s -> None
end
include M

let parse ~debug p s = 
  let s = string_to_state ~debug s in 
  run p s |> fun v -> 
  match v with
  | None -> None
  | Some(v,s) -> 
    if debug then begin             
      Printf.printf "run: max posn reached: %d; " !(s.max);
      Printf.printf "at that posn: %s\n" 
        (String.sub s.init_input !(s.max) (s.len - !(s.max)))
    end;
    Some v  (* NOTE not all the input need be consumed *)

let _ : debug:bool -> 'a m -> string -> 'a option = parse


let get_state () = inject @@ fun s -> Some(s,s)
let set_state s = inject @@ fun _ -> Some((),s)

(** Modify a parser so that it prints a message and the position in
   the input that has been reached before it tries to parse *)
let debug ~msg p = inject @@ fun s -> 
  msg s;
  run p s

(* may return < len chars *)
let get_substring len : string m = 
  get_state () >>= fun s ->   
  return (String.sub s.init_input s.i (if s.i+len <= s.len then len else s.len-s.i))
  

let skip n = 
  get_state () >>= fun s -> 
  assert(s.i + n <= s.len);
  set_state (update_max {s with i=s.i+n})

(* FIXME creates a substring *)
let _a s = 
  get_substring (String.length s) >>= fun s' -> 
  match s=s' with
  | true -> skip (String.length s)
  | false -> fail ()

(** This version does not allocate substrings *)
let a s = inject @@ fun st -> 
  let len_s = String.length s in
  match st.i + len_s <= st.len && 
        (0 |> iter_k (fun ~k n -> 
             match n >= len_s with
             | true -> true
             | false -> 
               (String.get s n = String.get st.init_input (st.i+n)) &&
               k (n+1)))
  with
  | true -> Some (s,update_max {st with i=st.i+len_s})
  | false -> None
    
let eps = a ""


(** Link to ocaml-re; we need to match against a string from a certain
   position. This is a thin layer over ocaml-re. NOTE: you can use
   everything in Re in addition to the following. NOTE: Re searches
   from the given position in the string; thus, the first match may
   not start at the given position in the string. But we require that
   it does start at that position. So we explicitly add "beginning of
   string". *)
module Re_ = struct
  type t = Re.t (* not compiled *)
  type re = Re.re (* compiled *)

  let compile: t -> re = Re.compile

  (** Return end position of matching substring (which starts at pos) *)
  let exec_opt ~pos re str : int option = 
    Re.exec_opt ~pos re str |> function
    | None -> None
    | Some g -> 
      Some(Re.Group.stop g 0)

  (** Return matching substring *)
  let exec_opt_s ~pos re str : string option =
    exec_opt ~pos re str |> function
    | None -> None
    | Some j -> Some(String.sub str pos (j-pos))

  (** Parser, returns matching substring *)
  let exec re = inject @@ fun s -> 
    exec_opt_s ~pos:s.i re s.init_input |> function
    | None -> None
    | Some mtch -> Some (mtch, update_max {s with i=s.i+(String.length mtch)})

  (** Parser, returns matching group; doesn't advance the position *)
  let exec_g re : Re.Group.t m = inject @@ fun s ->    
    Re.exec_opt ~pos:s.i re s.init_input |> function
    | None -> None
    | Some g -> Some(g,s)

  (* Alternative literal matching; surely this can't be faster than
     direct string comparison? *)
  let a s = Re.(seq [start;str s]) |> compile |> exec
end

let exec re : string m = Re_.exec re

(** Try to parse; return None if not possible *)
let opt p = inject @@ fun s -> 
  run p s |> function
  | None -> Some(None,s)
  | Some(x,s) -> Some(Some x,s)

let _ : 'a m -> 'a option m= opt

(** Prefer first option, but if not possible try second *)
let alt a b = 
  opt a >>= function 
  | None -> b
  | Some x -> return x

let rec alt_list xs = 
  match xs with
  | [] -> fail ()
  | x::xs -> 
    alt x (alt_list xs)

let end_of_input = inject @@ fun s -> 
  match s.i = s.len with 
  | true -> Some((),s)
  | false -> None

(** Parse one then another; return a pair *)
let then_ a b = a >>= fun x -> b >>= fun y -> return (x,y)

(** seq is just then_ *)
let seq = then_

let rec seq_list xs = 
  match xs with 
  | [] -> fail ()
  | [x] -> x >>= fun v -> return [v]
  | x::xs -> x >>= fun v -> seq_list xs >>= fun vs -> return (v::vs)

(** The variants seq3 etc allow more accurate typing than seq_list *)
let seq3 (p,q,r) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> return (x,y,z)

let seq4 (p,q,r,m) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> 
  m >>= fun z' -> 
  return (x,y,z,z')

let seq5 (p, q, r, m, n) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> 
  m >>= fun z' -> 
  n >>= fun z'' -> 
  return (x,y,z,z',z'')

let seq6 (p, q, r, m, n, o) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> 
  m >>= fun z' -> 
  n >>= fun z'' -> 
  o >>= fun z''' -> 
  return (x,y,z,z',z'',z''')

let seq7 (p, q, r, m, n, o, i) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> 
  m >>= fun z' -> 
  n >>= fun z'' -> 
  o >>= fun z''' -> 
  i >>= fun z4 -> 
  return (x,y,z,z',z'',z''',z4)

let seq8 (p, q, r, m, n, o, i, j) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> 
  m >>= fun z' -> 
  n >>= fun z'' -> 
  o >>= fun z''' -> 
  i >>= fun z4 -> 
  j >>= fun z5 ->   
  return (x,y,z,z',z'',z''',z4,z5)

let seq9 (p, q, r, m, n, o, i, j, k) = 
  p >>= fun x -> 
  q >>= fun y -> 
  r >>= fun z -> 
  m >>= fun z' -> 
  n >>= fun z'' -> 
  o >>= fun z''' -> 
  i >>= fun z4 -> 
  j >>= fun z5 ->   
  k >>= fun z6 ->   
  return (x,y,z,z',z'',z''',z4,z5,z6)

(** Repetition *)
let rep p = 
  let rec f acc = 
    opt p >>= function
    | None -> return (List.rev acc)
    | Some x -> f (x::acc)
  in
  f []

(** This variant returns the matching seps as well *)
let list_with_sep ~sep p = 
  opt p >>= function
  | None -> return `Empty
  | Some x -> 
    rep (then_ sep p) >>= function xs -> 
      return (`Not_empty(x,xs))

let _ : sep:'a m -> 'b m -> [`Empty | `Not_empty of 'b * ('a * 'b) list ] m = list_with_sep

(** This variant does not return the seps *)
let list ~sep p = 
  list_with_sep ~sep p >>= function
  | `Empty -> return []
  | `Not_empty (x,ys) -> return (x::(List.map snd ys))

let _ : sep:'a m -> 'b m -> 'b list m = list

    
let upto_a lit = 
  let re = Re.(shortest (seq [start; group (rep any); str lit]) |> compile) in
  Re_.exec_g re >>= fun g ->
  Re.Group.get g 1 |> fun s -> 
  skip (String.length s) >>= fun () -> 
  return s

(** Parse one of the given literals *)
let any_of xs = alt_list (List.map a xs)

(** Parse until reaching one of the given literals *)
let rep_any_but xs = 
  let re = Re.(shortest (seq [start; group (rep any); alt (List.map str xs)]) |> compile) in
  Re_.exec_g re >>= fun g ->
  Re.Group.get g 1 |> fun s -> 
  skip (String.length s) >>= fun () -> 
  return s

(** Whitespace; may be empty *)
let ws = exec Re.(compile (seq [start; rep (set "\t \n")]))

(** Whitespace; not empty *)
let ws1 = exec Re.(compile (seq [start; rep1 (set "\t \n")]))

(** Whitespace; may be empty; no new lines *)
let ws_nnl = exec Re.(compile (seq [start; rep (set "\t ")]))


module Test() = struct

  let num = exec Re.(compile (seq [start; longest (rep1 digit)]))

  let num = num >>= fun s -> return (int_of_string s)

  let list_of_num = 
    a"[" >>= fun _ -> 
    list ~sep:(a";") num >>= fun xs -> 
    a"]" >>= fun _ -> 
    return xs

  let debug=true

  let p_starts_with_upper = exec Re.(seq [start; rg 'A' 'Z';longest (rep alnum)] |> compile)

  let _ = 
    Printf.printf "%s: testing ... " __MODULE__;  
    assert(parse ~debug p_starts_with_upper "[" = None);
    assert(parse ~debug p_starts_with_upper "['-'] integerLiteral" = None);
    assert(parse ~debug ws "h" = Some "");
    assert(parse ~debug ws1 "h" = None);
    assert(parse ~debug ws "" = Some "");
    assert(parse ~debug ws " " = Some " ");
    assert(parse ~debug ws "  " = Some "  ");
    assert(parse ~debug (a "hello") "hello" = Some "hello");
    assert(parse ~debug (a "") "hello" = Some "");
    assert(parse ~debug list_of_num "[]" = Some []);
    assert(parse ~debug list_of_num "[1]" = Some [1]);
    assert(parse ~debug list_of_num "[123]" = Some [123]);
    assert(parse ~debug list_of_num "[1;2;3]" = Some [1;2;3]);
    Printf.printf "passed\n";
    ()

end

  
