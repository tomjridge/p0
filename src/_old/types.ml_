(** Simple monadic parsing type; at most one result *)
type 'a m = string -> ('a * string) option


(** Basic re ops *)
type 're re_ops = {
  literal: string -> 're;
  string_match: re:'re -> off:int -> string -> int option;
  search_forward: re:'re -> off:int -> string -> int option      
}


(** Parsers arising from re_ops *)
type 're re_parsers = {
  re: re:'re -> string m;
  upto_re: re:'re -> string m
}
