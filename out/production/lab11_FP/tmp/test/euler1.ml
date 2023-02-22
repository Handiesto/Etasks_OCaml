open! Recursion
open! Tailrecursion
open! Map
open! Infinity_list
open! OUnit2

let test_nth_prime name expected n =
  name >:: (fun _ -> assert_equal expected (Recursion.nth_prime n))

let test_nth_prime_map name expected n =
  name >:: (fun _ -> assert_equal expected (Map.nth_prime n))

let tests =
  "test suite for nth_prime" >::: [
    test_nth_prime "test 1 - Recursion" 80863 7915;
    test_nth_prime "test 2 - Recursion" 4517 613;
    test_nth_prime "test 3 - Recursion" 104743 10001;
    test_nth_prime "test 4 - Recursion" 58567 5927;
    test_nth_prime "test 5 - Recursion" 1794439 134677;
    test_nth_prime_map "test 1 - Map" 80863 7915;
    test_nth_prime_map "test 2 - Map" 4517 613;
    test_nth_prime_map "test 3 - Map" 104743 10001;
    test_nth_prime_map "test 4 - Map" 58567 5927;
    test_nth_prime_map "test 5 - Map" 1794439 134677;
  ]


let () =
  run_test_tt_main tests;
  print_endline "All tests passed.";
