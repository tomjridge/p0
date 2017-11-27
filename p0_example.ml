(* a cleaned up version of ocaml grammar 2017 *)

(* worth working with indexes rather than strings? *)

open P0

let re s = re (Str.regexp s)

let upto_re s = upto_re (Str.regexp s)

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

let test () = 
  let _ = grammar g |> function 
    | Some (x,s) -> 
      assert (if s="" then true else (print_endline s; false) ) 
    | None -> failwith __LOC__ 
  in
  Pervasives.print_string "Parsing grammar (x100)...";
  for i = 1 to 100 do ignore(grammar g) done;
  print_endline "finished!"

let _ = test ()

(*

$ time ./a.out
Parsing grammar (x100)...
finished!

real	0m0.228s
user	0m0.212s
sys	0m0.016s

*)


(* arithmetic ------------------------------------------------------- *)

(* NOTE the notion of precedence is here: a times is "upto" a non-times + *)

let rec times s = (plus ~sep:(a"*") atomic |>> fun es -> return (`Times es)) s

and plus_ s = (plus ~sep:(a"+") times |>> fun es -> return (`Plus es)) s

and atomic s = (
  num || 
  (a"(" -- plus_ -- a")" |>> fun ((_,x),_) -> return (`Bra x))) s

and num s = (re"[0-9]+" |>> fun x -> return (`Int (int_of_string x))) s


let test () = ("1+2*3*4+5*6" |> plus_)


(* csv -------------------------------------------------------------- *)

let dq = "\""
let comma = ","
let eol = "\n"
let rec inside sofar s = (
  upto_a dq -- a dq |>> fun (x,_) ->
  (opt (a dq) |>> function
    | None -> return (`Quoted (sofar^x))
    | Some _ -> inside (sofar^x^dq))) s
let quoted = (a dq -- inside "") |>> fun (_,x) -> return x
let unquoted_terminators = ("["^comma^dq^eol^"]")
(* NOTE the following will parse an empty line as an unquoted *)
let unquoted s = (
  upto_re unquoted_terminators |>> fun x -> return (`Unquoted x)) s
let field = quoted || unquoted
let row = plus ~sep:(a comma) field  (* see unquoted || (a"" |>> fun _ -> return []) *)
let rows = star ~sep:(a eol) row

let _ = 
  print_string "Testing csv parser...";
  rows {|
a,b,c
d,"e,f,g",h
i,"jk""l",
|} 
  |> fun res ->
  let expected  = Some
      ([[`Unquoted ""]; [`Unquoted "a"; `Unquoted "b"; `Unquoted "c"];
        [`Unquoted "d"; `Quoted "e,f,g"; `Unquoted "h"];
        [`Unquoted "i"; `Quoted "jk\"l"; `Unquoted ""]; [`Unquoted ""]],
       "")
  in
  assert(res=expected);
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
