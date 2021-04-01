(** Monadic parser combinators.

Return at most 1 parse at each point (so, alternatives are ordered,
   and the first applicable is taken).

A rewrite of 2017 version. *)


type state = {
  init_input : string;
  len        : int; (* of init_input *)
  i          : int;
  debug      : bool;
  mutable max: int; (* debug: this is the max i reached; updated in bind *)
}

let update_max s = s.max <- max s.max s.i

let string_to_state ~debug s = {
  init_input = s;
  len        = String.length s;
  i          = 0;
  debug;
  max        = 0;
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
        update_max t;
        ab a t
  let ( >>= ) = bind

  let run x s = x s
           
  let inject f = f
  let fail () = fun _s -> None
end
open M

let parse ~debug p s = 
  run p (string_to_state ~debug s) |> function
  | None -> None
  | Some(v,s) -> 
    if s.debug then begin             
      Printf.printf "run: max posn reached: %d; " s.max;
      Printf.printf "at that posn: %s\n" 
        (String.sub s.init_input s.max (s.len - s.max))
    end;
    Some v  (* NOTE not all the input need be consumed *)

let _ : debug:bool -> 'a m -> string -> 'a option = parse


let get_state () = inject @@ fun s -> Some(s,s)
let set_state s = inject @@ fun _ -> Some((),s)

(** Modify a parser so that it prints a message and the position in
   the input that has been reached before it tries to parse *)
let debug ~msg p = inject @@ fun s -> 
  Printf.printf "%s: %d\n" msg s.i;
  run p s

(* may return < len chars *)
let get_substring len : string m = 
  get_state () >>= fun s ->   
  return (String.sub s.init_input s.i (if s.i+len <= s.len then len else s.len-s.i))
  

let skip n = 
  get_state () >>= fun s -> 
  assert(s.i + n <= s.len);
  set_state {s with i=s.i+n}


let a s = 
  get_substring (String.length s) >>= fun s' -> 
  match s=s' with
  | true -> skip (String.length s)
  | false -> fail ()
    
let eps = a ""


(** Link to ocaml-re; we need to match against a string from a certain
   position. This is a thin layer over ocaml-re. NOTE: you can use
   everything in Re in addition to the following. *)
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
    | Some mtch -> Some (mtch, {s with i=s.i+(String.length mtch)})

  (** Parser, returns matching group; doesn't advance the position *)
  let exec_g re : Re.Group.t m = inject @@ fun s ->    
    Re.exec_opt ~pos:s.i re s.init_input |> function
    | None -> None
    | Some g -> Some(g,s)

  (* Alternative literal matching; surely this can't be faster than
     direct string comparison? *)
  let a s = Re.str s |> compile |> exec
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

let end_of_string = inject @@ fun s -> 
  match s.i = s.len with 
  | true -> Some((),s)
  | false -> None

(** Parse one then another; return a pair *)
let then_ a b = a >>= fun x -> b >>= fun y -> return (x,y)

(** Repetition *)
let rep p = 
  let rec f acc = 
    opt p >>= function
    | None -> return (List.rev acc)
    | Some x -> f (x::acc)
  in
  f []

let list ~sep p = 
  opt p >>= function
  | None -> return []
  | Some x -> 
    let rec f acc = 
      opt (then_ sep p) >>= function
      | None -> return (List.rev acc)
      | Some (_,x) -> f (x::acc)
    in
    f [x]

let _ : sep:'a m -> 'b m -> 'b list m = list

(** This variant returns the matching seps as well *)
let list_with_sep ~sep p = 
  opt p >>= function
  | None -> return `Empty
  | Some x -> 
    rep (then_ sep p) >>= function xs -> 
      return (`Not_empty(x,xs))

let _ : sep:'a m -> 'b m -> [`Empty | `Not_empty of 'b * ('a * 'b) list ] m = list_with_sep
    
let upto_a lit = 
  let re = Re.(shortest (seq [group (rep any); str lit]) |> compile) in
  Re_.exec_g re >>= fun g ->
  Re.Group.get g 1 |> fun s -> 
  skip (String.length s) >>= fun () -> 
  return s



module Test() = struct

  let num = exec Re.(compile (longest (rep1 digit)))

  let num = num >>= fun s -> return (int_of_string s)

  let list_of_num = 
    a"[" >>= fun _ -> 
    list ~sep:(a";") num >>= fun xs -> 
    a"]" >>= fun _ -> 
    return xs

  let debug=true

  let _ = 
    Printf.printf "%s: testing ... " __MODULE__;  
    assert(parse ~debug list_of_num "[]" = Some []);
    assert(parse ~debug list_of_num "[1]" = Some [1]);
    assert(parse ~debug list_of_num "[123]" = Some [123]);
    assert(parse ~debug list_of_num "[1;2;3]" = Some [1;2;3]);
    Printf.printf "passed\n";
    ()

end

  
