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

(* Tests *)
let test() =
  let expected = [0; 1; 2] in
  let actual = lexicographic_permutation 0 [0; 1; 2] in
  assert (expected = actual);

  let expected = [0; 2; 1] in
  let actual = lexicographic_permutation 1 [0; 1; 2] in
  assert (expected = actual);

  let expected = [2; 1; 0] in
  let actual = lexicographic_permutation 5 [0; 1; 2] in
  assert (expected = actual);

  let expected = [3; 1; 2; 0] in
  let actual = lexicographic_permutation 5 [0; 1; 2; 3] in
  assert (expected = actual);

  let expected = [9; 8; 7; 6; 5; 4; 3; 2; 1; 0] in
  let actual = lexicographic_permutation 3628800 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);
