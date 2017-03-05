
open OUnit2
open Avltree

let basic_member_test test_ctxt =
   let tree = make_tree [1;2;3] in
   assert_bool "member failed to find 1 in [1;2;3]" (member tree 1)

let basic_negative_member_test test_ctxt =
   let tree = make_tree [1;2;3] in
   assert_bool "member found 4 in [1;2;3]" (not (member tree 4))

let member_suite =
   "member_suite" >:::
      [ "basic_member_test" >:: basic_member_test
      ; "basic_negative_member_test" >:: basic_negative_member_test
      ]

let insert_cases =
   [ "single_insert", [1], Tree(1, 1, Empty, Empty)
   ; "double_insert", [1;2], Tree(1, 2, Empty, Tree(2, 1, Empty, Empty))
   ; "left_rotation", [1; 2; 3], Tree(2, 2, Tree(1, 1, Empty, Empty),
                                            Tree(3, 1, Empty, Empty))
   ; "right_rotation", [3; 2; 1], Tree(2, 2, Tree(1, 1, Empty, Empty),
                                             Tree(3, 1, Empty, Empty))
   ; "left_right_rotation", [3; 1; 2], Tree(2, 2, Tree(1, 1, Empty, Empty),
                                                  Tree(3, 1, Empty, Empty))
   ; "right_left_rotation", [1; 3; 2], Tree(2, 2, Tree(1, 1, Empty, Empty),
                                                  Tree(3, 1, Empty, Empty))
   ; "deep_left_rotation", [1; 2; 3; 4; 5], Tree(2, 3, Tree(1, 1, Empty, Empty),
                                                       Tree(4, 2, Tree(3, 1, Empty, Empty),
                                                                  Tree(5, 1, Empty, Empty)))
   ; "deep_right_left_rotation", [1; 2; 3; 5; 4], Tree(2, 3, Tree(1, 1, Empty, Empty),
                                                             Tree(4, 2, Tree(3, 1, Empty, Empty),
                                                                        Tree(5, 1, Empty, Empty)))
   ; "deep_right_rotation", [5; 4; 3; 2; 1], Tree(4, 3, Tree(2, 2, Tree(1, 1, Empty, Empty),
                                                                   Tree(3, 1, Empty, Empty)),
                                                        Tree(5, 1, Empty, Empty))
   ; "deep_left_rotation", [5; 4; 3; 1; 2], Tree(4, 3, Tree(2, 2, Tree(1, 1, Empty, Empty),
                                                                  Tree(3, 1, Empty, Empty)),
                                                       Tree(5, 1, Empty, Empty))
   ]

let insert_suite = "insert_suite" >:::
   List.map (fun (name, input, output) ->
               let msg = Printf.sprintf "%s failed" name in
               name >:: (fun test_ctxt ->
                  assert_equal (make_tree input) output ~msg:msg))
            insert_cases

let () =
   run_test_tt_main member_suite;
   run_test_tt_main insert_suite

