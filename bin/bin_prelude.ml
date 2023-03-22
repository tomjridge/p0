(** Helper to avoid dependence on associativity of -- *)
let _3 ((x1,x2),x3) = (x1,x2,x3)

(** For pretty-printing *)
module Internal = struct
  module Grammar_type = struct
    open Core
    type nt = string [@@deriving sexp]
    type tm = Tm_lit of (string * string * string) | Tm_qu of string [@@deriving sexp]
    type sym = Nt of nt | Tm of tm [@@deriving sexp]
    type rhs = sym list [@@deriving sexp]
    type rule = nt * rhs list [@@deriving sexp]
    type grammar = rule list [@@deriving sexp]

    let grammar_to_string g = 
      g |> sexp_of_grammar |> Core.Sexp.to_string_hum 
  end
  let grammar_to_string = Grammar_type.grammar_to_string

  (** Make pretty-printing slightly more human by omitting some brackets *)
  module Grammar_human = struct
    include struct 
      open Core
      type nt = string [@@deriving sexp]
      type sym = string [@@deriving sexp]
      type rhs = sym list [@@deriving sexp]
      type rule = nt * rhs list [@@deriving sexp]
      type grammar = rule list [@@deriving sexp]
    end

    let of_grammar (g:Grammar_type.grammar) : grammar = 
      let open Grammar_type in 
      let rec conv_g = function
        | rs -> List.map conv_rule rs
      and conv_rule (nt,rhs) = (nt,List.map conv_rhs rhs)
      and conv_rhs syms = List.map conv_sym syms 
      and conv_sym = function
        | Nt nt -> nt
        | Tm (Tm_lit (s1,s2,s3)) -> s2 (* s1^s2^s3 *)
        | Tm (Tm_qu s) -> s
      in
      conv_g g

    (* but strings with quotes are still double quoted *)

    let grammar_to_string g = of_grammar g |> sexp_of_grammar |> Core.Sexp.to_string_hum
  end
  let grammar_to_string = Grammar_human.grammar_to_string
end


(** For pretty-printing *)
module Arith_type = struct
  open Core
  type arith = 
      Times of arith list
    | Plus of arith list
    | Atomic of atomic  [@@deriving sexp]
  and atomic = Int of int | Bracket of arith [@@deriving sexp]
  let arith_to_string x = 
    x |> sexp_of_arith |> Core.Sexp.to_string_hum 
end
