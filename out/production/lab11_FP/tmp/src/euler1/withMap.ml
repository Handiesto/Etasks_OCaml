let is_prime n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

let nth_prime n =
  let next_primes primes =
    match primes with
    | [] -> [2]
    | hd :: _ ->
      let candidates = List.map (fun x -> hd + x) [2; 4; 6; 2; 4; 6; 6; 2; 6; 4; 2; 4; 4; 4; 8; 6; 6] in
      let next = List.find is_prime candidates in
      next :: primes
  in
  let primes = ref [] in
  for _ = 1 to n do
    primes := next_primes !primes
  done;
  List.hd !primes

let result = nth_prime 10001
let () = print_int result; print_newline ()
