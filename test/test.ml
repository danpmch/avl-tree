
open OUnit2

let test1 test_ctxt =
   let tree = Avltree.make_tree [1;2;3] in
   assert_equal true (Avltree.member tree 1)

let member_suite =
   "member_suite">:::
      ["test1">:: test1]

let () =
   run_test_tt_main member_suite

