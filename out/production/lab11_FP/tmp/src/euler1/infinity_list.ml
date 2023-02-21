open! Lazy

let rec from n =
  lazy (n :: (Lazy.force (from (n+1))))

let rec primes_from n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d+0))
  in
  if is_not_divisor 2 then lazy (n :: Lazy.force (primes_from (n+1)))
  else primes_from (n+1)

let primes = primes_from 2

let nth_prime n =
  let rec nth_prime_helper n primes =
    match Lazy.force primes with
    | hd :: tl -> if n = 1 then hd else nth_prime_helper (n-1) (lazy tl)
    | [] -> failwith "unexpected empty lis"
  in
  nth_prime_helper n primes

let () =
  print_int (nth_prime 10001);
  print_newline ()
