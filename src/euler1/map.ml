let rec map f lst =
  match lst with
  | [] -> []
  | x :: xs -> f x :: map f xs

let is_prime n =
  let limit = int_of_float (sqrt (float_of_int n)) in
  let rec is_not_divisor d =
    d > limit || (n mod d <> 0 && is_not_divisor (d+1))
  in n > 1 && is_not_divisor 2

let rec range a b =
  if a > b then []
  else a :: range (a+1) b

let nth_prime n =
  let rec find_nth_prime i p =
    match i = n, is_prime (p + 1) with
    | true, _ -> p
    | _, true -> find_nth_prime (i + 1) (p + 1)
    | _ -> find_nth_prime i (p + 1)
  in find_nth_prime 1 2

let result =
  let nums = map (fun x -> x+2) (range 0 9999) in
  nth_prime (List.hd (List.rev nums))
(*let () = print_int (result)*)