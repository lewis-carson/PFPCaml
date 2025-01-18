open OUnit2
open Test_family

let suite =
  "Probability Tests" >::: [
    "test_prob_two_boys_given_boy" >:: test_prob_two_boys_given_boy
  ]

let () =
  run_test_tt_main suite
