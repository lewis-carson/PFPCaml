open OUnit2
open Prob.Probability

let boy = "boy"
let girl = "girl"

let family = uniform [ (boy, boy); (boy, girl); (girl, boy); (girl, girl) ]

let has_boy (x, y) = x = boy || y = boy

let two_boys (x, y) = x = boy && y = boy

let prob_two_boys_given_boy =
  let D d = family ||| has_boy in
  let filtered_d = List.filter (fun ((x, y), _) -> two_boys (x, y)) d in
  P (List.fold_left (fun acc (_, p) -> acc +. p) 0.0 filtered_d)

let test_prob_two_boys_given_boy _ =
  let P p = prob_two_boys_given_boy in
  assert_equal ~printer:string_of_float 0.3333333333333333 p
