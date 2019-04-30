
(** Monadic parser combinators *)

module Internal = struct

  module type I = sig
    type t 
    val drop : int -> t -> t
    val len : t -> int
  end

  module type RE = sig
    module I : I
    type re (* Re.t *)
    type compiled_re (* Re.re *) 

    val literal: I.t -> re
    val longest: re -> re

    val compile: re -> compiled_re

    type group
    val group_stop : group -> int  (* use 0 group match *)
    val exec_opt : compiled_re -> I.t -> group option  
    (* we have to decide how to process the input given information about the match *)
  end

  module Make(I:I)(Re:RE with module I := I) = struct

    open I
    open Re

    module Monad : sig
      type 'a m
      val return : 'a -> 'a m
      val ( >>=) : 'a m -> ('a -> 'b m) -> 'b m

      val of_fun: (I.t -> ('a * I.t) option) -> 'a m
      val to_fun: 'a m -> (I.t -> ('a * I.t) option)

      val get_input: unit -> I.t m
      val set_input: I.t -> unit m
    end = struct
      type 'a m = I.t -> ('a * I.t) option

      let bind (x:'a m) (f:'a -> 'b m) :'b m = 
        fun s -> x s |> function | None -> None | Some (v,s) -> f v s
      let ( >>= ) x f = bind x f

      let return x s = Some(x,s)

      let of_fun x = x
      let to_fun x = x

      let get_input () = fun s -> Some(s,s)
      let set_input s = fun _ -> Some((),s)
    end

    open Monad

    let exec_re : compiled_re -> group m = fun cre -> of_fun (fun i -> 
      Re.exec_opt cre i |> function
      | None -> None
      | Some g -> Some(g,i))

    let exec_and_drop : compiled_re -> group m = fun cre -> of_fun (fun i -> 
      Re.exec_opt cre i |> function
      | None -> None
      | Some g -> Some(g,i |> drop (group_stop g)))      


    (** An example of how to use regexps with monad *)
    let a (lit0:I.t) : I.t m = 
      let len = len lit0 in
      let lit = compile (literal lit0) in
      exec_re lit >>= fun _g -> 
      get_input () >>= fun i ->
      drop len i |> set_input >>= fun () ->
      return lit0

    (** Parse an optional something *)
    let opt p : 'a m = of_fun (fun i ->
      match (to_fun p) i with
      | None -> Some(None,i)
      | Some (x,i) -> Some(Some x,i))

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
    let end_of_string = of_fun (fun i -> 
      match len i with 
      | 0 -> Some((),i)
      | _ -> None)

  end

  module StringI = struct
    type t = string
    let drop n s = String.sub s n (String.length s - n)
    let len s = String.length s
  end

  module Re_ = struct
    open Re
    type re = Re.t
    type compiled_re = Re.re
    let literal s = str s
    let longest re = longest re

    let compile = Re.compile

    type group = Re.Group.t
    let group_stop g = Group.stop g 0
    let exec_opt re i = Re.exec_opt re i
  end

  module Ocaml_re_instance = struct 
    include (struct include StringI end : module type of StringI with type t:=string)
    include Re_
    include Make(StringI)(Re_)
  end

(*
sig
  type re = Re_.re
  type compiled_re = Re_.compiled_re
  val literal : string -> re
  val longest : re -> re
  val compile : re -> compiled_re
  type group = Re_.group
  val group_stop : group -> int
  val exec_opt : compiled_re -> string -> group option
  module Monad :
    sig
      type 'a m = 'a Make(StringI)(Re_).Monad.m
      val return : 'a -> 'a m
      val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
      val of_fun : (string -> ('a * string) option) -> 'a m
      val to_fun : 'a m -> string -> ('a * string) option
      val get_input : unit -> string m
      val set_input : string -> unit m
    end
  val exec_re : compiled_re -> group Monad.m
  val exec_and_drop : compiled_re -> group Monad.m
  val a : string -> unit Monad.m
  val opt : 'a Monad.m -> 'a option Monad.m
  val then_ : 'a Monad.m -> 'b Monad.m -> ('a * 'b) Monad.m
  val ( -- ) : 'a Monad.m -> 'b Monad.m -> ('a * 'b) Monad.m
  val plus : sep:'a Monad.m -> 'b Monad.m -> 'b list Monad.m
  val star : sep:'a Monad.m -> 'b Monad.m -> 'b list Monad.m
  val alt : 'a Monad.m -> 'a Monad.m -> 'a Monad.m
  val ( || ) : 'a Monad.m -> 'a Monad.m -> 'a Monad.m
  val end_of_string : unit Monad.m
end

*)

  module Export : sig
    val drop : int -> string -> string
    val len : string -> int

    type re = Re.t (* Re_.re *)
    type compiled_re = Re.re (* Re_.compiled_re *)
    val literal : string -> re
    val longest : re -> re
    val compile : re -> compiled_re
    type group = Re.Group.t
    val group_stop : group -> int
    val exec_opt : compiled_re -> string -> group option

    type 'a m 
    val return : 'a -> 'a m
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val of_fun : (string -> ('a * string) option) -> 'a m
    val to_fun : 'a m -> string -> ('a * string) option
    val get_input : unit -> string m
    val set_input : string -> unit m

    val exec_re : compiled_re -> group m
    val exec_and_drop : compiled_re -> group m
    val a : string -> string m
    val opt : 'a m -> 'a option m
    val then_ : 'a m -> 'b m -> ('a * 'b) m
    val ( -- ) : 'a m -> 'b m -> ('a * 'b) m
    val plus : sep:'a m -> 'b m -> 'b list m
    val star : sep:'a m -> 'b m -> 'b list m
    val alt : 'a m -> 'a m -> 'a m
    val ( || ) : 'a m -> 'a m -> 'a m
    val end_of_string : unit m
  end = struct include Ocaml_re_instance include Monad end
end

include Internal.Export

let upto_a lit = 
  Re.(shortest (seq [group (rep any); str lit])) |> Re.compile |> exec_re >>= fun g ->
  Re.Group.get g 1 |> fun s -> 
  (* Printf.printf "upto_a: %s\n%!" s; *)
  get_input () >>= fun i -> drop (String.length s) i |> set_input >>= fun () ->
  return s

(** Convert an [Re.t] to a parser returning a string; uses Re group 0; uses bos and longest *)
let re_to_p (re: Re.t) : string m = 
  let re = compile Re.(seq [bos; longest re]) in  
  exec_re re >>= fun g ->
  Re.Group.get g 0 |> fun s -> 
  get_input () >>= fun i -> drop (String.length s) i |> set_input >>= fun () ->
  return s

  
