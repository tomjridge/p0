open P0_lib

(** Helper to avoid dependence on associativity of -- *)
let _3 ((x1,x2),x3) = (x1,x2,x3)

(* a cleaned up version of ocaml grammar 2017 *)

(* grammar of grammars ---------------------------------------------- *)

let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)

(* includes comments *)
let ws = 
  let ws_regexp = Re.(rep (set " \n")) in
  let ws = re ws_regexp in
  let rec f () = 
    ws >>= fun _ ->
    opt (comm >>= fun _ -> f ()) >>= fun _ -> return ()
  in
  f ()

(** This just to avoid type var error msgs *)
module Grammar_type = struct
  open Core_kernel
  type nt = string [@@deriving sexp]
  type tm = Tm_lit of (string * string * string) | Tm_qu of string [@@deriving sexp]
  type sym = Nt of nt | Tm of tm [@@deriving sexp]
  type rhs = sym list [@@deriving sexp]
  type rule = nt * rhs list [@@deriving sexp]
  type grammar = rule list [@@deriving sexp]

  let grammar_to_string g = 
    g |> sexp_of_grammar |> Core_kernel.Sexp.to_string_hum 
end
let grammar_to_string = Grammar_type.grammar_to_string

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

  let bar = 
    re (Re.char '|') |> fun bar ->
    ws -- bar -- ws 

  let rhs = plus ~sep:bar syms

  let rule = 
    let arrow = ws -- a "->" -- ws in
    nt -- arrow -- rhs >>= fun ((nt,_),rhs) -> 
    return (nt,rhs)

  let rules = 
    let sep = ws -- a";" -- ws in
    star ~sep rule 

  let grammar = 
    ws -- rules -- ws >>= fun ((_,rs),_) -> return rs

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
    Printf.printf "Grammar example parsed successfully (%s)\n%s\n\n%!" __FILE__ (grammar_to_string g)


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
    | Some (_x,s) -> 
      assert (if s="" then true else (print_endline s; false) ) 
    | None -> failwith __LOC__ 
  in
  Pervasives.print_string "Parsing grammar (x100)...";
  for _i = 1 to 100 do ignore(to_fun grammar g) done;
  print_endline "finished!"

let _ = test ()

(*

Parsing grammar (x100)...finished!

real	0m0.199s
user	0m0.166s
sys	0m0.027s

*)

(*
(* arithmetic ------------------------------------------------------- *)

(* NOTE the notion of precedence is here: a times is "upto" a non-times + *)

let rec times s = (plus ~sep:(a"*") atomic >>= fun es -> return (`Times es)) s

and plus_ s = (plus ~sep:(a"+") times >>= fun es -> return (`Plus es)) s

and atomic s = (
  num || 
  (a"(" -- plus_ -- a")" >>= fun ((_,x),_) -> return (`Bra x))) s

and num s = (re"[0-9]+" >>= fun x -> return (`Int (int_of_string x))) s


let test () = ("1+2*3*4+5*6" |> plus_)


(* csv -------------------------------------------------------------- *)

let dq = "\""
let comma = ","
let eol = "\n"
let rec inside sofar s = (
  upto_a dq -- a dq >>= fun (x,_) ->
  (opt (a dq) >>= function
    | None -> return (`Quoted (sofar^x))
    | Some _ -> inside (sofar^x^dq))) s
let quoted = (a dq -- inside "") >>= fun (_,x) -> return x
let unquoted_terminators = ("["^comma^dq^eol^"]")
(* NOTE the following will parse an empty line as an unquoted provided
   followed by an unquoted terminator; an empty field as an unquoted

   FIXME we may want unquoted to also parse the empty line upto the end of string
   *)
let unquoted s = (
  upto_re unquoted_terminators >>= fun x -> return (`Unquoted x)) s
(* the last unquoted field can extend to the end of the string,
   provided no terminators are found *)
let last_unquoted_field s = 
  match search_forward ~re:(Str.regexp unquoted_terminators) ~off:0 s with
  | None -> Some(`Unquoted s,"")
  | Some _ -> None  
let field = quoted || unquoted || last_unquoted_field
let row = plus ~sep:(a comma) field  (* see unquoted || (a"" >>= fun _ -> return []) *)
let rows = star ~sep:(a eol) row

let _ = 
  print_string "Testing csv parser...";
  assert(
  rows {|
a,b,c
d,"e,f,g",h
i,"jk""l",
|} = Some([
        [`Unquoted ""]; [`Unquoted "a"; `Unquoted "b"; `Unquoted "c"];
        [`Unquoted "d"; `Quoted "e,f,g"; `Unquoted "h"];
        [`Unquoted "i"; `Quoted "jk\"l"; `Unquoted ""];
        [`Unquoted ""]
      ],
        ""));
  
  assert(
    rows {|
a,b,c
d,e,f|} = Some([
        [`Unquoted ""];
        [`Unquoted "a"; `Unquoted "b"; `Unquoted "c"];
        [`Unquoted "d"; `Unquoted "e"; `Unquoted "f"]],"")); 
  print_endline "finished!"


(* NOTE need Tjr_substring
module Manual_testing_ = functor(_: sig end) -> struct
  let test = {|"jk""l"|} |> quoted

  let _ = unquoted "\n"

  let _ = 
    Tjr_substring.(
      Str.(
        upto_re ~re:(regexp "x") {s_="x";i_=0})) [@@warning "-40"]

  let _ = (upto_re unquoted_terminators "hx\n")

  let test = (unquoted {|h
                       |}) |> fun x -> x;;

  let test = row {|d,"e,f,g",h
                 |}

  (* tuareg doesn't seem to like the string syntax - so eval region *)
  let test = rows {|
a,b,c
d,"e,f,g",h
|}

  let test = rows {|
a,b,c
d,"e,f,g",h
i,"jk""l",
|} 
end
*)
*)
