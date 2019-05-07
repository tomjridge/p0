
(** Monadic parser combinators *)


(** State-passing monad, with option *)
module Monad : sig 
  type ('a,'t) m
  val return: 'a -> ('a,'t)m
  val bind: ('a,'t)m -> ('a -> ('b,'t) m) -> ('b,'t)m

  val ( >>= ) : ('a,'t)m -> ('a -> ('b,'t) m) -> ('b,'t)m
      
  val fail : unit -> ('a,'t) m

  (** Reveal the implementation type *)
  module Internal : sig
    val of_fun : ('t -> ('a * 't)option) -> ('a,'t)m
    val to_fun : ('a,'t)m -> ('t -> ('a * 't)option)
  end
end = struct
  type ('a,'t)m = ('t -> ('a * 't)option)
  let return a = (fun t -> Some(a,t))
  let bind a ab = 
    fun t -> a t |> function
      | None -> None
      | Some (a,t) ->
        ab a t

  let ( >>= ) = bind

  let fail () = fun _t -> None

  module Internal = struct
    let of_fun f = f
    let to_fun m = m
  end

end
open Monad



module Internal = struct

  (** Inputs eg strings *)
  module type I = sig
    type t 
    val drop : int -> t -> t
    val split_at: int -> t -> t*t
    val len : t -> int
  end

  (** integrate input with monad *)
  module type I_WITH_MONAD = sig
    module I : I
    type t  (* the monad state type *)
    val get_input: unit -> (I.t,t) m
    val set_input: I.t -> (unit,t) m
  end

  (** NOTE we assume that re matches from the beginning of the string
     (and probably for the longest we can match). For ocaml-re, this
     means we have to match against seq[bos;re] *)
  module type RE = sig
    module I : I
    type re (* Re.t *)
    type compiled_re (* Re.re *) 

    val literal: I.t -> re

    (** NOTE only way to get compiled_re; named awkwardly to emphasize
       the implicit bos and longest *)
    val compile_bos_longest: re -> compiled_re

    type group
    val group_stop : group -> int  (* use 0 group match *)
    val exec_opt : compiled_re -> I.t -> group option  
    (* we have to decide how to process the input given information
       about the match *)
  end

  module Make(I:I)(IM:I_WITH_MONAD with module I:=I)(Re:RE with module I:=I) = struct

    open I
    open IM
    open Re

    type 'a m = ('a,t) Monad.m
        
    (** lift a function on inputs to the monad *)
    let _of_fun: (I.t -> ('a*I.t)option) -> 'a m = fun f -> 
      get_input () >>= fun i -> 
      f i |> function
      | None -> fail ()
      | Some(a,i) -> 
      set_input i >>= fun () ->
      return a

    (** run in the *)
    (* let to_fun: 'a m -> (I.t -> ('a * I.t) option *)


    (** This is not used often; NOTE that there is no bos/longest *)
    let raw_exec_cre_no_drop : compiled_re -> group m = 
      fun cre -> _of_fun (fun i -> 
          Re.exec_opt cre i |> function
          | None -> None
          | Some g -> Some(g,i))

    let raw_exec_cre_and_drop : compiled_re -> (I.t * group) m = 
      fun cre -> _of_fun (fun i -> 
          Re.exec_opt cre i |> function
          | None -> None
          | Some g -> 
            let len = group_stop g in
            split_at len i |> fun (i1,i2) -> 
            Some((i1,g),i2))

    (** Execute a regular expression and return the matched string;
       uses bos and longest (and group 0 via group_stop) *)
    let re : re -> I.t m = fun re ->
      let cre = compile_bos_longest re in  
      raw_exec_cre_and_drop cre >>= fun (s,_) -> 
      return s

    (** An example of how to use regexps with monad FIXME does this
       match at bos? *)
    let a (lit0:I.t) : I.t m = 
      let lit = compile_bos_longest (literal lit0) in
      raw_exec_cre_and_drop lit >>= fun _ -> 
      return lit0

    (** Parse an optional something *)
    let opt p : 'a m = 
      Monad.Internal.to_fun p |> fun p ->
      Monad.Internal.of_fun @@
      fun t -> p t |> function
        | None -> Some(None,t)
        | Some(a,t) ->Some(Some a,t)  

    (** Parse one then another; return a pair *)
    let then_ a b = a >>= fun x -> b >>= fun y -> return (x,y)

    (** Infix for then_ *)
    let ( -- ) = then_

    (** Parse one or more *)
    let rec plus ~sep p = 
      p >>= fun x ->
      (opt (sep -- plus ~sep p)) >>= function
      | None -> return [x]
      | Some (_,xs) -> return (x::xs)

    (** Zero or more *)
    let star ~sep p =
      opt p >>= function
      | None -> return []
      | Some x -> 
        opt (sep -- plus ~sep p) >>= function
        | None -> return [x]
        | Some (_,xs) -> return (x::xs)

    (** Shortcut alternative *)
    let alt a b = 
      opt a >>= function
      | None -> b
      | Some x -> return x

    (** Infix alt *)
    let ( || ) = alt       

    (** Matches a length-zero remaining input *)
    let end_of_string = _of_fun (fun i -> 
      match len i with 
      | 0 -> Some((),i)
      | _ -> None)

    (** NOTE assumes alternatives not empty *)
    let rec alternatives = function
      | [] -> failwith "no alternatives"
      | [x] -> x
      | x::xs -> x || alternatives xs

    (** NOTE assumes seq not empty *)
    let rec sequence = function
      | [] -> failwith __LOC__
      | [x] -> x >>= fun r -> return [r]
      | x::xs -> x -- (sequence xs) >>= fun (a,b) -> return (a::b)
  end

  module StringI = struct
    type t = string
    let drop n s = String.sub s n (String.length s - n)
    let len s = String.length s
    let split_at n s = 
      assert(n<=String.length s);
      (String.sub s 0 n, drop n s)
  end

  module StringIM = struct
    module I = StringI
    type t = I.t  (* the monad state is just the input string *)
    let get_input () = Monad.Internal.of_fun (fun t -> 
        Some(t,t))
    let set_input i = Monad.Internal.of_fun (fun _t ->
        Some((),i))
  end

  module Re_ = struct
    open Re
    type re = Re.t
    type compiled_re = Re.re
    let literal s = str s
    let longest re = longest re

    let compile_bos_longest re = Re.compile (seq[bos;longest re])

    type group = Re.Group.t
        
    (** NOTE uses group 0 *)
    let group_stop g = Group.stop g 0
    let exec_opt re i = Re.exec_opt re i
  end

  module Ocaml_re_instance = struct 
    include (struct include StringI end : module type of StringI with type t:=string) (* get rid of type t defn *)
    include StringIM
    include Re_
    include Make(StringI)(StringIM)(Re_)

  end

(*
sig
  val drop : int -> string -> string
  val len : string -> int
  val split_at : int -> string -> string * string
  module I = StringI
  type t = string
  val get_input : unit -> ('a, 'a) m
  val set_input : 'a -> (unit, 'b) m
  type re = Re_.re
  type compiled_re = Re_.compiled_re
  val literal : t -> re
  val longest : re -> re
  val compile_bos_longest : re -> compiled_re
  type group = Re_.group
  val group_stop : group -> int
  val exec_opt : compiled_re -> t -> group option
  type 'a m = ('a, t) Monad.m
  val of_fun : (t -> ('a * t) option) -> 'a m
  val raw_exec_cre_no_drop : compiled_re -> group m
  val raw_exec_cre_and_drop : compiled_re -> (t * group) m
  val re : re -> t m
  val a : t -> t m
  val opt : ('a, t) Monad.m -> 'a option m
  val then_ : ('a, 'b) Monad.m -> ('c, 'b) Monad.m -> ('a * 'c, 'b) Monad.m
  val ( -- ) : ('a, 'b) Monad.m -> ('c, 'b) Monad.m -> ('a * 'c, 'b) Monad.m
  val plus : sep:('a, t) Monad.m -> ('b, t) Monad.m -> ('b list, t) Monad.m
  val star : sep:('a, t) Monad.m -> ('b, t) Monad.m -> ('b list, t) Monad.m
  val alt : ('a, t) Monad.m -> ('a, t) Monad.m -> ('a, t) Monad.m
  val ( || ) : ('a, t) Monad.m -> ('a, t) Monad.m -> ('a, t) Monad.m
  val end_of_string : unit m
end
*)

  module Export : sig

    (** {2 String utils} *)

    val drop : int -> string -> string
    val len : string -> int
    val split_at : int -> string -> string * string
    (* type t = string *)


    (** {2 Regular expressions} *)
    
    type re = Re.t (* Re_.re *)
    (* type compiled_re = Re.re (\* Re_.compiled_re *\) *)
    val literal : string -> re
    val longest : re -> re
    (* val compile_bos_longest : re -> compiled_re *)

    type group = Re.Group.t
    val group_stop : group -> int
    (* val exec_opt : compiled_re -> string -> group option *)


    (** {2 Monad type and ops} *)

    type 'a m = ('a, string) Monad.m
    val return : 'a -> 'a m
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val of_fun : (string -> ('a * string) option) -> 'a m
    val to_fun : 'a m -> string -> ('a * string) option
    val get_input : unit -> string m
    val set_input : string -> unit m

    (** {2 Regular expressions and the monad} *)

    (* val raw_exec_cre_no_drop : compiled_re -> group m *)
    (* val raw_exec_cre_and_drop : compiled_re -> (string * group) m *)

    (** The main interface to regular expressions *)
    val re : re -> string m

    (** {2 Standard combinators} *)

    val a : string -> string m
    val opt : 'a m -> 'a option m
    val then_ : 'a m -> 'b m -> ('a * 'b) m
    val ( -- ) : 'a m -> 'b m -> ('a * 'b) m
    val plus : sep:'a m -> 'b m -> 'b list m
    val star : sep:'a m -> 'b m -> 'b list m
    val alt : 'a m -> 'a m -> 'a m
    val ( || ) : 'a m -> 'a m -> 'a m
    val end_of_string : unit m
    val alternatives : 'a m list -> 'a m
    val sequence : 'a m list -> 'a list m

    (** {2 Support for OCaml's Str regexp lib} *)

    val str_re : string -> string m
  end = struct
    include Ocaml_re_instance 
    let of_fun = _of_fun
    let ( >>= ) = Monad.( >>= )
    let return = Monad.return
    let to_fun = Monad.Internal.to_fun

    let str_re (re:string) = 
      let open Str in
      let re = Str.regexp re in
      of_fun (fun s -> 
          string_match re s 0 |> function
          | false -> None
          | true -> matched_string s |> fun mat -> 
                    Some(mat,drop (String.length mat) s))
    let _ = str_re

  end
end

include Internal.Export


(** {2 Misc, specific to string monad? FIXME} *)

let upto_a lit = 
  let cre = Re.(shortest (seq [group (rep any); str lit])) |> Re.compile in
  cre |> Internal.Ocaml_re_instance.raw_exec_cre_no_drop >>= fun g ->
  Re.Group.get g 1 |> fun s -> 
  (* Printf.printf "upto_a: %s\n%!" s; *)
  get_input () >>= fun i -> drop (String.length s) i |> set_input >>= fun () ->
  return s


(** debug by showing the input (and an optional msg) *)  
let debug ?(msg="") () = of_fun (fun i ->
  Printf.printf "%s %s\n%!" msg i; Some((),i))
