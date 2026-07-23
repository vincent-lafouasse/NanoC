type error =
  | TooBig of string (* the original input, magnitude too large for int64 *)
  | TooSmall of string (* the original input, magnitude too small for int64 *)
[@@deriving show]

let _assert cond message : unit = if cond then () else failwith message

(* always leave at least one digit
   e.g. "000000" -> "0"
*)
let strip_leading_zeros (s : string) : string =
  let n = String.length s in
  let rec first_nonzero i =
    if i = n - 1 then i else if s.[i] = '0' then first_nonzero (i + 1) else i
  in
  let i = first_nonzero 0 in
  String.sub s i (n - i)
;;

(* input are digits only, no sign
   they are compared by length
   if same length, lexicographically
*)
let compare_magnitudes (a : string) (b : string) : int =
  let len_a = String.length a in
  let len_b = String.length b in
  if len_a <> len_b then Int.compare len_a len_b else String.compare a b
;;

let int64_max_magnitude = Int64.to_string Int64.max_int

let int64_min_magnitude =
  let s = Int64.to_string Int64.min_int in
  String.sub s 1 (String.length s - 1)
;;

(* no risky wrapping conversions by using lexicographic ordering to check for
   int min/max before converting/accumulating
*)
let atoi64 (s : string) : (int64, error) result =
  let is_negative, magnitude =
    if String.length s > 0 && s.[0] = '-'
    then true, String.sub s 1 (String.length s - 1)
    else false, s
  in
  _assert (String.length magnitude > 0) "atoi64: no digits";
  let magnitude = strip_leading_zeros magnitude in
  let bound = if is_negative then int64_min_magnitude else int64_max_magnitude in
  if compare_magnitudes magnitude bound > 0
  then Error (if is_negative then TooSmall s else TooBig s)
  else Ok (Int64.of_string s)
;;
