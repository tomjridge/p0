open P0_lib
open Bin_prelude

(* grammar of grammars ---------------------------------------------- *)

let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)

let _whitespace = re Re.(rep (set " \n")) 

(* the following idiom allows to define a recursive parser that
   satisfies OCaml; eta expansion using underlying type of monad as a
   function *)
(* includes comments *)
let rec ws s = 
  to_fun
    (_whitespace -- opt (comm -- of_fun ws) >>= fun _ -> return ())
    s
let ws = of_fun ws

open Internal

(** Parse a textual grammar defn (the example below is the defn of the
   grammar used in the OCaml manual, which is used to express the
   grammar of OCaml itself) *)
module Grammar_of_grammars = struct 
  open Grammar_type
  let nt = re Re.(rep1 (rg 'A' 'Z'))

  let tm_lit = 
    let sq = "'" in
    let dq = "\"" in
    ((a sq -- upto_a sq -- a sq) ||
     (a dq -- upto_a dq -- a dq))
    >>= fun x -> return (_3 x)

  let tm_re = re Re.(seq [char '?';rep1 (alt [alnum;char '_'])])

  let tm = 
    (tm_lit >>= fun x -> Tm_lit(x) |> return) ||
    (tm_re >>= fun y -> Tm_qu(y) |> return)

  let sym = 
    (nt >>= fun x -> return (Nt x)) || 
    (tm >>= fun x -> return (Tm x))

  let syms = plus ~sep:ws sym

  let rhs = plus ~sep:(ws -- a"|" -- ws) syms

  let rule = nt -- (ws -- a "->" -- ws) -- rhs 
    >>= fun ((nt,_),rhs) -> return (nt,rhs)

  let rules = star ~sep:(ws -- a";" -- ws) rule 

  let grammar = ws -- rules -- ws >>= fun ((_,rs),_) -> return rs

  let _ : (nt * sym list list) list m = grammar
end

let grammar = Grammar_of_grammars.grammar


(* example ---------------------------------------------------------- *)

let example = {|

(* the expressions we want to parse at top-level *)
S -> ?w DEFN ?w ?eof
| ?w TYPEDEFINITIONS ?w ?eof
| ?w TYPEXPR ?w ?eof

|} 

let _ = 
  to_fun grammar example |> function
  | None -> failwith __LOC__
  | Some(g,_) -> 
    Printf.printf "Grammar example parsed successfully (%s)\n%s\n\n%!" 
      __FILE__ (grammar_to_string g)


(* ocaml grammar ---------------------------------------------------- *)


(* FIXME really the variable names tend to be created from the names
   of the nonterminals/terminals involved eg w1 for first ?w, mp for
   MODULEPATH etc; this suggests to optionally decorate each nt/tm
   with a base var name *)

let g = {|

S -> ?w SS ?w ?eof;

(* the expressions we want to parse at top-level *)
SS -> DEFN
| TYPEDEFINITIONS
| TYPEXPR
| "e:" EXPR
| "val" ?w EXPR ?w ":" ?w TYPEXPR
;

DEFN -> 
    "let" ?w LETBINDING 
  | "let" ?w "rec" ?w LETBINDING 
;

TYPEDEFINITIONS -> TYPEDEFINITION 
  | TYPEDEFINITION ?w TYPEDEFINITIONS   
;


(* var names: DEFN d; TYPEDEFINITIONS tds; *)


(* 6.3 Names -------------------------------------------------------- *)

VALUENAME -> ?ident;

INFIXOP -> "="
  | "++"
  | "::"
  | "<"
  | "<="

  | "+"
  | "@"
  | "&&"
  | "||"
  | "***>"
  | "||||"
  | ">>>>"
  | ">>="
  | "|||"
;

FIELDNAME -> ?ident ;

VALUEPATH -> VALUENAME
  | MODULEPATH "." VALUENAME    
;

(* this was ?ident but we don't want List.map interpreted as
CONSTR.FIELDNAME *)

CONSTR ->
    ?constr
;

MODULEPATH -> ?_Ident
  | MODULEPATH "." ?_Ident
;


(* var names: VALUENAME vn; FIELDNAME fn; VALUEPATH vp; MODULEPATH
mp *)

(* 6.4 Type expressions --------------------------------------------- *)

(* following for type defns FIXME needs tidying
up *)

TYPEXPR -> "'" ?ident
  | "(" ?w TYPEXPR ?w ")"
  | TYPEXPR ?w "->" ?w TYPEXPR
  | TYPEXPR ?w "*" ?w TYPEXPR
  | TYPECONSTR
  | TYPECONSTR ?w TYPEXPRA
;

TYPEXPRA -> TYPEXPR
  | TYPEXPR ?w TYPEXPRA
;

OCAMLTYPEXPR -> "'" ?ident
  | "(" ?w TYPEXPR ?w ")"
  | TYPEXPR ?w "->" ?w TYPEXPR
  | TYPEXPR ?w "*" ?w TYPEXPR
  | TYPECONSTR
  | TYPEXPR ?w TYPECONSTR
  | "(" ?w TYPEXPRA ?w ")" ?w TYPECONSTR
;

OCAMLTYPEXPRA -> TYPEXPR
  | TYPEXPR ?w "," ?w TYPEXPRA
;

(*
  | "(" ?w TYPE ?w ")"
  | TYPE ?w ?ident
  | MODULEPATH "." TYPEXPR
  | TYPEXPR ?w TYPECONSTR
*)

TYPECONSTR -> TYPECONSTRNAME
  | MODULEPATH "." TYPECONSTRNAME
;

POLYTYPEXPR -> TYPEXPR
;

(* var names FIXME *)


(* 6.5 Constants ---------------------------------------------------- *)

CONSTANT -> CONSTR
  | "[" ?w "]"
  | ?num
  | "-" ?num
  | '"' ?notdquote '"'
  | "'" ?notsquote "'"
  | "()"
;



(* 6.6 Patterns ----------------------------------------------------- *)

PATTERN -> EXPR
;


(* 6.7 Expressions -------------------------------------------------- *)


(* grammar is too ambiguous so we identify atomic
expressions which can be arguments to functions *)

EXPR -> ATEXPR
  | EXPR ":" TYPEXPR
  | EXPR ?w "," ?w EXPRA
  | CONSTR ?w EXPR
  | EXPR ?w "::" ?w EXPR
  | FNARGS
  | EXPR ?w INFIXOP ?w EXPR
  | "if" ?w EXPR ?w "then" ?w EXPR ?w "else" ?w EXPR

  | "match" ?w EXPR ?w "with" ?w PATTERNMATCHING ?w "end"

  | "let" ?w LETBINDING ?w "in" ?w EXPR

  | "let" ?w "rec" ?w LETBINDING ?w "in" ?w EXPR

  | "fun" ?w MULTIPLEMATCHING
;

(* FIXME List.map parses as ATEXPR "." FIELDNAME, where ATEXPR is CONSTANT CONSTR *)

ATEXPR ->
    VALUEPATH
  | CONSTANT
  | "(" ?w EXPR ?w ")"
  | "[" ?w EXPRLIST ?w "]"
  | RECORD
  | ATEXPR "." FIELDNAME
  | ATEXPR "." "[" EXPR "]"
  | "_"
  | "<fun>"
  | "(" ?w INFIXOP ?w ")"
;

EXPRA -> EXPR
  | EXPR ?w "," ?w EXPRA
;


EXPRLIST ->
    EXPR
  | EXPR ?w ";" ?w EXPRLIST
;


PATTERNMATCHING -> CASESB
  | "|" ?w CASESB
;


CASESB ->
  CASE
  | CASE ?w "|" ?w CASESB
(* above clause erroneously allows cases to start with a bar *)
;

CASE -> PATTERN ?w "->" ?w EXPR
;


MULTIPLEMATCHING -> PATTERNMATCHING ;

LETBINDING -> PATTERN ?w "=" ?w EXPR ;



FNARGS -> ATEXPR ?w ATEXPR
  | ATEXPR ?w FNARGS
;

RECORD ->
    "<|" ?w FIELDS ?w "|>"
  | "<|" ?w ATEXPR ?w "with" ?w FIELDS ?w "|>"
;

FIELDS ->
    FIELD
  | FIELD ?w ";" ?w FIELDS
;

FIELD ->
    ?ident ?w "=" ?w EXPR
;


(* 6.8 Type and exception definitions ------------------------------- *)

TYPEDEFINITION -> "type" ?w TYPEDEF
;

TYPEDEF -> TYPECONSTRNAME ?w TYPEINFORMATION

  | TYPECONSTRNAME ?w TYPEPARAMS ?w TYPEINFORMATION

(* FIXME what about params? may cause problems because hol lists
params in order they occur in defn :( *)
;

TYPEPARAMS -> TYPEPARAM
  | TYPEPARAM ?w TYPEPARAMS
;

OCAMLTYPEPARAMS -> TYPEPARAM
  | "(" ?w TYPEPARAMSA ?w ")"
;

TYPEPARAMSA -> TYPEPARAM
  | TYPEPARAM ?w "," ?w TYPEPARAMSA
;

TYPEPARAM -> "'" ?ident
;

TYPECONSTRNAME -> ?ident
;

TYPEINFORMATION -> TYPEEQUATION
  | TYPEREPRESENTATION
;

TYPEEQUATION -> "=" ?w TYPEXPR ;

TYPEREPRESENTATION -> "=" ?w CONSTRDECLS
  | "=" ?w "<|" ?w FIELDDECLS ?w "|>"
;


CONSTRDECLS -> CONSTRDECL
  | CONSTRDECL ?w "|" ?w CONSTRDECLS
;

CONSTRDECL -> CONSTRNAME
  | CONSTRNAME ?w "of" ?w TYPEXPR
;

CONSTRNAME -> ?_Ident ;

(* following can end in optional ; *)
FIELDDECLS -> FIELDDECLSA
  | FIELDDECLSA ?w ";"
;

FIELDDECLSA -> FIELDDECL
  | FIELDDECL ?w ";" ?w FIELDDECLSA
;

(* FIXME the pattern is we map nonterms to strings; this causes lots
of messing about with c and ^ *)

FIELDDECL -> FIELDNAME ?w ":" ?w POLYTYPEXPR 

|}

let test () = 
  let _ = to_fun grammar g |> function 
    | Some (g,s) -> 
      assert (if s="" then true else (print_endline s; false) );
      g |> grammar_to_string |> print_endline
    | None -> failwith __LOC__ 
  in
  Printf.printf "Parsing grammar (x100)...%!";
  let f () = 
    for _i = 1 to 100 do ignore(to_fun grammar g) done
  in
  Bin_util.time f |> fun dur -> 
  Printf.printf "finished in %G seconds!\n\n%!" dur

let _ = test ()

(*
Parsing grammar (x100)...finished in 0.152814 seconds!
*)

(* arithmetic ------------------------------------------------------- *)

(* NOTE the notion of precedence is here: a times is "upto" a non-times + *)


let arith_to_string = Arith_type.arith_to_string

include struct
  open Arith_type

  let num = re (Re.(rep1 digit)) >>= fun x -> return (Int (int_of_string x))

  (* eta expand sum so that we can call recursively *)

  let rec sum s = 
    let bracket = a"(" -- of_fun sum -- a")"  >>= fun ((_,x),_) -> return (Bracket x) in
    let atomic = (num || bracket) >>= fun x -> return (Atomic x) in
    let product = plus ~sep:(a"*") atomic >>= fun es -> return (Times es) in
    to_fun 
      (plus ~sep:(a"+") product >>= fun es -> return (Plus es))
      s

  let arith = of_fun sum

  let _ = arith

end

let test () = 
  (arith |> to_fun) "1+2*3*4+5*6" |> fun (Some(a,"")) -> 
  Printf.printf "Arithmetic example: \n%s\n\n%!" (a |> arith_to_string)

let _ = test()


(* csv -------------------------------------------------------------- *)

(* chars *)
module C = struct
  let dq = '"'
  let comma = ','
  let eol = '\n'
end

module R = struct
  open C
  let dq = re Re.(char dq)
  let not_dq = re Re.(rep (compl [char C.dq]))
  let comma = re Re.(char comma)
  let eol = re Re.(char C.eol)
end
open R

(** For pretty-printing *)
module Csv_type = struct
  open Core_kernel      
  type field = Q of string | U of string [@@deriving sexp]
  type row = field list [@@deriving sexp]
  type csv = row list [@@deriving sexp]
  let csv_to_string x =
    x |> sexp_of_csv |> Core_kernel.Sexp.to_string_hum 
end

open Csv_type

(** A CSV parser. NOTE dq is "double quote" *)
module Csv_parser = struct
  (** [inside] matches the inside of a quoted field; two consecutive
      double quotes encodes a single dquote *)
  let rec inside sofar =
    not_dq >>= fun s ->
    dq >>= fun _ ->
    opt dq >>= function
    | None -> return (Q (sofar^s))
    | Some _ -> inside (sofar^s^String.make 1 C.dq)

  let quoted = (dq -- inside "") >>= fun (_,x) -> return x

  let unquoted_terminator = Re.(alt[char '"'; char ','; char '\n'])

  let unquoted = 
    re Re.(rep (compl [unquoted_terminator])) >>= fun s ->
    return (U s)

  let field = quoted || unquoted 

  let row = plus ~sep:comma field 

  let rows = star ~sep:eol row
end


let rows = Csv_parser.rows

let test_csv csv_as_string expected = 
  assert(to_fun rows csv_as_string |> function
  | None -> false
  | Some (c,_) -> c=expected)

let _ = 
  Printf.printf "Example csv parse: %s\n\n%!" 
    (to_fun rows {|
a,b,c|} |> fun (Some(csv,_)) -> csv_to_string csv)

let _ = 
  print_string "Testing csv parser...";
  test_csv 
    {|
a,b,c|} 
    [
      [U ""]; 
      [U "a"; U "b"; U "c"];
    ];

  test_csv 
    {|
a,b,c
d,"e,f,g",h
i,"jk""l",
|} 
    [
      [U ""]; 
      [U "a"; U "b"; U "c"];
      [U "d"; Q "e,f,g"; U "h"];
      [U "i"; Q "jk\"l"; U ""];
      [U ""]
    ];

  print_endline "finished!"
