let rec lexicographic_permutation n lst =
  match lst with
  | [] -> []
  | _ ->
    let fact = List.length lst - 1 |> factorial in
    let idx = n / fact in
    let rest = n mod fact in
    let elem = List.nth lst idx in
    elem :: lexicographic_permutation rest (List.filter (fun x -> x <> elem) lst)

and factorial n =
  match n with
  | 0 | 1 -> 1
  | _ -> n * factorial (n - 1)

let solve () =
  let perm = lexicographic_permutation 999999 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.iter print_int perm;
  print_newline ()

let () = solve ()
