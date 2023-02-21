let is_prime n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

let rec nth_prime n i count =
  match count = n, is_prime i with
  | true, _ -> i
  | false, true -> nth_prime n (i + 1) (count + 1)
  | false, false -> nth_prime n (i + 1) count

let result = nth_prime 632 2 0
(*let () = print_int (result-1)*)