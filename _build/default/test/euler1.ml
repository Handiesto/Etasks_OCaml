open! Recursion
open! Tailrecursion
open! Map
open! Infinity_list
let test() =
   let expected = 80863 in
   let actual = infinity_list.nth_prime 7915 in
   assert (expected = actual);

   let expected = 4517 in
   let actual = Recursion.nth_prime 613 in
   assert (expected = actual);

   let expected = 104743 in
   let actual = Recursion.nth_prime 10001 in
   assert (expected = actual);

   let expected = 58567 in
   let actual = Recursion.nth_prime 5927 in
   assert (expected = actual);

   let expected = 1794439 in
   let actual = Recursion.nth_prime 134677 in
   assert (expected = actual);

   let expected = 80863 in
   let actual = Map.nth_prime 7915 in
   assert (expected = actual);

   let expected = 4517 in
   let actual = Map.nth_prime 613 in
   assert (expected = actual);

   let expected = 104743 in
   let actual = Map.nth_prime 10001 in
   assert (expected = actual);

   let expected = 58567 in
   let actual = Map.nth_prime 5927 in
   assert (expected = actual);

   let expected = 1794439 in
   let actual = Map.nth_prime 134677 in
   assert (expected = actual);

    print_string("Tests passed...");
    ()

let () = test();