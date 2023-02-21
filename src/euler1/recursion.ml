let is_prime n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

let nth_prime n =
  let rec search i count =
    match count = n, is_prime i with
    | true, _ -> i
    | false, true -> search (i+1) (count+1)
    | false, false -> search (i+1) count
  in search 2 0 - 1

let result = nth_prime 10001
(*let () = print_int (result);
print_newline()*)


