(* Computes the nth lexicographic permutation of a list *)
let lexicographic_permutation n lst =
  let rec aux n acc lst =
    match lst with
    | [] -> List.rev acc
    | _ ->
      let fact = List.length lst - 1 |> factorial in
      let idx = n / fact in
      let rest = n mod fact in
      let elem = List.nth lst idx in
      aux rest (elem :: acc) (List.filter (fun x -> x <> elem) lst)
  (* Add the 'rec' keyword to define a recursive function *)
  and factorial n =
    let rec aux acc n =
      match n with
      | 0 | 1 -> acc
      | _ -> aux (acc * n) (n - 1)
    in
    aux 1 n
  in
  aux n [] lst

(* Computes and prints the solution *)
let solve () =
  let perm = lexicographic_permutation 999999 [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
    List.iter print_int perm;
    print_newline ()

(* Runs the solution *)
let () = solve ()
