let rec permutations lst = match lst with
  | [] -> [[]]
  | hd::tl -> List.concat (List.map (fun perm ->
      let rec insert x lst = match lst with
        | [] -> [[x]]
        | hd::tl -> (x::lst) :: List.map (fun y -> hd::y) (insert x tl)
      in insert hd perm) (permutations tl))

let lexicographic_permutation n =
  let digits = List.init 10 (fun x -> string_of_int x) in
  let perms = permutations digits in
  let nth_perm = List.nth perms (n - 1) in
  String.concat "" nth_perm

let answer = lexicographic_permutation 1000000