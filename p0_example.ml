(* a cleaned up version of ocaml grammar 2017 *)

(* worth working with indexes rather than strings? *)

open P0

(* grammar of grammars ---------------------------------------------- *)

let comm = a "(*" -- upto_a "*)" -- a "*)"  (* FIXME nested comments *)
(* ws: 0 or more; re is hopefully longest match (not true for Str) *)
let rec ws s = (re "[ \n]*") --- (opt (comm --- ws)) @@ s
let nt = re "[A-Z]+" 
let lit = 
  let sq = "'" in
  let dq = "\"" in
  ((a sq -- upto_a sq -- a sq) ||
  (a dq -- upto_a dq -- a dq))
  |>> fun x -> return (_3 x)
let tm = 
  lit |>> (fun x -> return @@ `Lit x) ||
  (* allow a single question mark for terminals *)
  (a"?" -- re"[a-z_][a-zA-Z0-9]*") |>> fun (x,y) -> return @@ `Qu(x,y)
let sym = 
  (nt |>> fun x -> return (`NT x)) || 
  (tm |>> fun x -> return (`TM x))
(*
let var_eq = 
  let v = re "[a-z][a-z0-9]*" in
  let v_eq = v -- a"=" in
  opt v_eq -- sym |>> fun (v,s) -> return (v,s)
*)
let syms = plus ~sep:ws sym
(* let vnames = plus ~sep:(a",") (re"[a-z_][a-z0-9_]*")
//let syms' = syms -- opt (ws -- a"//" -- opt (ws -- vnames)) *)
let bar = ws -- a "|" -- ws 
let rhs = plus ~sep:bar syms  (* plus and star are greedy *)
let rule = 
  sym -- (ws -- a "->" -- ws) -- rhs |>> fun x -> 
  _3 x |> fun (sym,_,rhs) -> return (sym,rhs)
let rules = star ~sep:(ws -- a";" -- ws) rule 
let grammar = ws -- rules -- ws |>> fun x -> _3 x |> fun (_,x2,_) -> return x2


(* example ---------------------------------------------------------- *)

module X_ = functor(_:sig end) -> struct

  let example = {|?w?|}

  let _ = tm example

  let _ = example |> a "?" -- re"[a-z]+"

  let example = {|

(* the expressions we want to parse at top-level *)
S -> ?w? DEFN ?w? ?eof?
| ?w? TYPEDEFINITIONS ?w? ?eof?
| ?w? TYPEXPR ?w? ?eof?

|}

  let _ = grammar example

end

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

let _ = Pervasives.print_string "Parsing grammar (x100)..."

let g' = for i = 1 to 100 do ignore(grammar g) done

let _ = grammar g |> function 
  | Some (x,s) -> 
    assert (if s="" then true else (print_endline s; false) ) 
  | None -> failwith __LOC__ 

let _ = print_endline "finished!"

(*

$ time ./a.out
Parsing grammar (x100)...
finished!

real	0m0.228s
user	0m0.212s
sys	0m0.016s

*)


(* arithmetic ------------------------------------------------------- *)

(* for plus, we need one 1+|exp| of lookahead to check the operation *)


let rec times init = 
  plus ~sep:(a"*") atomic |>> fun es -> return @@ `Times (init::es)

(* 1st clause; parses 1+2+3; needs at least one at exp, or empty *)
and plus1 sofar = (* zero or more *)
  (atomic |>> fun x -> plus2 sofar x)
  || ((a"") |>> fun _ -> return @@ `Plus sofar)

and plus2 sofar init = (
  opt (a"+") |>> function
  | Some _ -> plus1 (sofar@[init])  (* pass along accumulated terms *)
  | None -> opt (a"*") |>> function
    | Some _ -> (
        (* parse a product *)
        times init |>> fun product -> 
        (* then try to carry on parsing plus2 *)
        plus2 sofar product)
    | None -> 
      (* neither a + or a * *)
      return (`Plus3(sofar,init)))

and atomic s = 
  begin
    num || ( 
      a"(" -- plus1 [] -- a")" |>> fun ((_,x),_) -> return (`Bra x))
  end s

and num = re"[0-9]+" |>> fun x -> return @@ `Int (int_of_string x)

let _ = "1+2*3*4+5*6" |> plus1 [] |> function Some(x,_) -> 
    assert (x = `Plus3 ([`Int 1; `Times [`Int 2; `Int 3; `Int 4]], `Times [`Int 5; `Int 6]))