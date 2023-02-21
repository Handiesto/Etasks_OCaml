open! Recursion
open! Tailrecursion
open! Map
open! Infinity_list
open! OUnit2

let test_nth_prime name expected n =
  name >:: (fun _ -> assert_equal expected (Recursion.nth_prime n))
let test_nth_prime2 name expected n =
  name >:: (fun _ -> assert_equal expected (Tailrecursion.nth_prime n))
let test_nth_prime3 name expected n =
  name >:: (fun _ -> assert_equal expected (Map.nth_prime n))

let tests =
  "test suite for nth_prime" >::: [
    test_nth_prime "test 1" 80863 7915;
    test_nth_prime "test 2" 4517 613;
    test_nth_prime "test 3" 104743 10001;
    test_nth_prime "test 4" 58567 5927;
    test_nth_prime "test 5" 1794439 134677;
(*   test_nth_prime2 "test 1" 80863 7915;
    test_nth_prime2 "test 2" 4517 613;
    test_nth_prime2 "test 3" 104743 10001;
    test_nth_prime2 "test 4" 58567 5927;
    test_nth_prime2 "test 5" 1794439 134677;
    test_nth_prime3 "test 1" 80863 7915;
    test_nth_prime3 "test 2" 4517 613;
    test_nth_prime3 "test 3" 104743 10001;
    test_nth_prime3 "test 4" 58567 5927;
    test_nth_prime3 "test 5" 1794439 134677;*)
  ]

let () =
  run_test_tt_main tests;
  print_endline "All tests passed.";
