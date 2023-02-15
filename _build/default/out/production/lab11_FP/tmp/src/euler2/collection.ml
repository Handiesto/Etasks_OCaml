(* Returns the lexicographically next permutation of a list of integers *)
let next_permutation lst =
  let rec find_next_pivot i j = match lst.(i), lst.(j) with
    | _, _ when i = 0 -> None
    | x, y when x < y -> Some i
    | _, _ -> find_next_pivot (i-1) (j-1) in
  let rec swap i j = if i < j then (lst.(i) <- lst.(j); lst.(j) <- lst.(i); swap (i+1) (j-1)) in
  let rec reverse i j = if i < j then (lst.(i) <- lst.(j); lst.(j) <- lst.(i); reverse (i+1) (j-1)) in
  let n = Array.length lst in
  match find_next_pivot (n-2) (n-1) with
  | None -> None
  | Some pivot ->
    let successor = ref (n-1) in
    while lst.(!successor) <= lst.(pivot) do
      successor := !successor - 1
    done;
    swap pivot !successor;
    reverse (pivot+1) (n-1);
    Some lst

(* Returns the nth lexicographic permutation of a list of characters *)
let lexicographic_permutation n chars =
  let rec loop i lst = match next_permutation lst with
    | None -> failwith "Not enough permutations"
    | Some p -> if i = n then p else loop (i+1) p in
  let lst = Array.of_list (List.sort compare chars) in
  let result = loop 1 lst in
  List.map Char.escaped (Array.to_list result)
