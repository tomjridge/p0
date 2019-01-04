let starts_with ~prefix s =
  let len_p = String.length prefix in
  let len_s = String.length s in
  len_p > len_s |> function 
  | true -> false
  | false -> 
    let rec f j = 
      if j >= len_p then true else
        prefix.[j] = s.[j] &&
        f (j+1)
    in
    f 0

let starts_with_at_offset ~prefix ~offset s =
  let len_p = String.length prefix in
  let len_s = String.length s in
  offset + len_p > len_s |> function 
  | true -> false
  | false -> 
    let rec f j = 
      if j >= len_p then true else
        prefix.[j] = s.[offset+j] &&
        f (j+1)
    in
    f 0
  

(* more efficient version? *)
let drop n s =
  String.length s |> fun l ->
  if n >= l then "" else
    String.sub s n (l-n)

let split_at s n = 
  (String.sub s 0 n, String.sub s n (String.length s - n))

let test () = 
  assert(starts_with ~prefix:"tom" "tom");
  assert(starts_with ~prefix:"tom" "to" = false);
  assert(starts_with ~prefix:"tom" "t" = false);
  assert(starts_with ~prefix:"tom" "" = false);
  assert(starts_with ~prefix:"tom" "toma" = true);
  assert(starts_with_at_offset ~prefix:"tom" ~offset:0 "toma" = true);
  assert(starts_with_at_offset ~prefix:"tom" ~offset:1 "atom" = true);
  assert(starts_with_at_offset ~prefix:"tom" ~offset:1 "atoma" = true);
  assert(starts_with_at_offset ~prefix:"tom" ~offset:1 "ato" = false);
  assert(starts_with_at_offset ~prefix:"tom" ~offset:1 "ato" = false);
  assert(starts_with_at_offset ~prefix:"tom" ~offset:5 "ato" = false);
  ()

