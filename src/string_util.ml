let starts_with ~prefix b =
  let len = String.length prefix in
  len > String.length b |> function 
  | true -> false
  | false -> 
    let rec f j = 
      if j >= len then true else
        prefix.[j] = b.[j] &&
        f (j+1)
    in
    f 0

(* more efficient version? *)
let drop n s =
  String.length s |> fun l ->
  if n >= l then "" else
    String.sub s n (l-n)

let split_at s n = (String.sub s 0 n, String.sub s n (String.length s - n))
