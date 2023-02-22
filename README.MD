Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №1__

по Функциональному программированию

Выполнили: Плясов К. Е.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2023 г.

---
OCAML euler problems
=====

### First problem

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
### Implementation of the algorithm using tail recursion
```ocaml
let is_prime n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

let rec nth_prime n i count =
  if count = n then i
  else if is_prime i then nth_prime n (i + 1) (count + 1)
  else nth_prime n (i + 1) count

let result = nth_prime 632 2 0
```

### Implementations of the algorithm using recursion
```ocaml
let is_prime n =
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  n <> 1 && is_not_divisor 2

let nth_prime n =
  (let rec search i count =
    if count = n then i
    else if is_prime i then search (i + 1) (count + 1)
    else search (i + 1) count
  in
  search 2 0) - 1

let result = nth_prime 10001
```

### Implementations of the algorithm using map
```ocaml
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
    if i = n then p
    else if is_prime (p + 1) then find_nth_prime (i + 1) (p + 1)
    else find_nth_prime i (p + 1)
  in find_nth_prime 1 2
```

### Second problem

A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210 

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
### Implementations of the algorithm using recursion
```ocaml
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
```

### Implementations of the algorithm using tail recursion
```ocaml
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

  and factorial n =
    let rec aux acc n =
      match n with
      | 0 | 1 -> acc
      | _ -> aux (acc * n) (n - 1)
    in
    aux 1 n
  in
  aux n [] lst
```