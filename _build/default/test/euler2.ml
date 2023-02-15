open! Recursion2
open! Tailrecursion2

(* Tests *)

let test() =
  let expected = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let actual = Recursion2.lexicographic_permutation 0 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 1; 2; 3; 8; 7; 5; 6; 9; 4] in
  let actual = Recursion2.lexicographic_permutation 561 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 4; 5; 1; 3; 6; 7; 9; 2; 8] in
  let actual = Recursion2.lexicographic_permutation 136234 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [1; 5; 9; 8; 0; 3; 4; 2; 7; 6] in
  let actual = Recursion2.lexicographic_permutation 563791 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [2; 4; 1; 0; 7; 5; 9; 8; 3; 6] in
  let actual = Recursion2.lexicographic_permutation 852166 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [2; 7; 8; 3; 9; 1; 5; 4; 6; 0] in
  let actual = Recursion2.lexicographic_permutation 999999 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let actual = Tailrecursion2.lexicographic_permutation 0 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 1; 2; 3; 8; 7; 5; 6; 9; 4] in
  let actual = Tailrecursion2.lexicographic_permutation 561 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [0; 4; 5; 1; 3; 6; 7; 9; 2; 8] in
  let actual = Tailrecursion2.lexicographic_permutation 136234 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [1; 5; 9; 8; 0; 3; 4; 2; 7; 6] in
  let actual = Tailrecursion2.lexicographic_permutation 563791 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [2; 4; 1; 0; 7; 5; 9; 8; 3; 6] in
  let actual = Tailrecursion2.lexicographic_permutation 852166 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  let expected = [2; 7; 8; 3; 9; 1; 5; 4; 6; 0] in
  let actual = Tailrecursion2.lexicographic_permutation 999999 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  assert (expected = actual);

  print_string("Tests passed...");
  ()

  let () = test();