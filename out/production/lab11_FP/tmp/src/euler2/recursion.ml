(* Computes the nth lexicographic permutation of a list *)
let rec lexicographic_permutation n lst =
  match lst with
  | [] -> []
  | _ ->
    let fact = List.length lst - 1 |> factorial in
    let idx = n / fact in
    let rest = n mod fact in
    let elem = List.nth lst idx in
    elem :: lexicographic_permutation rest (List.filter (fun x -> x <> elem) lst)

(* Computes the factorial of a non-negative integer *)
and factorial n =
  match n with
  | 0 | 1 -> 1
  | _ -> n * factorial (n - 1)

(* Computes and prints the solution *)
let solve () =
  let perm = lexicographic_permutation 999999 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.iter print_int perm;
  print_newline ()

(* Runs the solution *)
let () = solve ()




(* Tests *)
let test() =
  let expected = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let actual = lexicographic_permutation 0 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 1; 2; 3; 8; 7; 5; 6; 9; 4] in
  let actual = lexicographic_permutation 561 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 4; 5; 1; 3; 6; 7; 9; 2; 8] in
  let actual = lexicographic_permutation 136234 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [1; 5; 9; 8; 0; 3; 4; 2; 7; 6] in
  let actual = lexicographic_permutation 563791 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [2; 4; 1; 0; 7; 5; 9; 8; 3; 6] in
  let actual = lexicographic_permutation 852166 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [2; 7; 8; 3; 9; 1; 5; 4; 6; 0] in
  let actual = lexicographic_permutation 999999 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  print_string("Tests passed...");
  ()

let () =
  solve ();
  test ();
