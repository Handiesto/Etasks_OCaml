(* Returns the factorial of a given integer *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

(* Returns the nth lexicographic permutation of a list of digits *)
let lexicographic_permutation digits n =
  let rec aux digits n acc =
    match digits with
    | [] -> List.rev acc
    | _ ->
      let f = factorial (List.length digits - 1) in
      let q, r = n / f, n mod f in
      let x = List.nth digits q in
      aux (List.filter (fun y -> y <> x) digits) r (x :: acc)
  in
  aux digits (n - 1) []

(* Computes the solution to the problem *)
let solve () =
  let digits = [0;1;2;3;4;5;6;7;8;9] in
  let n = 1000000 in
  let result = lexicographic_permutation digits n in
  List.iter print_int result;
  print_newline ()

(* Runs the solution *)
let () = solve ()