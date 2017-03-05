
type height = int
type 'a avl_tree = Empty
                 | Tree of 'a * height * 'a avl_tree * 'a avl_tree

let get_height = function
   Empty -> 0
 | Tree(_, h, _, _) -> h

let get_balance = function
   Empty -> 0
 | Tree (_, _, l, r) -> -(get_height l) + (get_height r)

let is_unbalanced tree =
   let balance = get_balance tree in
   balance < -1 || 1 < balance

let new_tree x = Tree(x, 1, Empty, Empty)

let rec find tree x = match tree with
   Empty -> None
 | Tree (e, _, left, right) as node -> if e = x then Some node
                                       else if e < x then find left x
                                       else find right x

(* this function is in the standard library, but I'm having some standard library
 * woes at the moment *)
let is_some = function
   None -> false
 | _ -> true

let rec member tree x =
   let node = find tree x in
   is_some node

let rotate_left = function
   Tree(x, h, t1,
              Tree(z, h', t23, t4)) ->
                 let h1 = get_height t1 in
                 let h23 = get_height t23 in
                 let h4 = get_height t4 in
                 let newH' = 1 + (max h1 h23) in
                 let newH = 1 + (max newH' h4) in
                 Tree(z, newH, Tree(x, newH', t1, t23), t4)
 | _ -> failwith "Error, unexpected tree configuration for rotate left"

let rotate_right = function
   Tree(x, h, Tree (z, h', t1, t23),
              t4) ->
                 let h1 = get_height t1 in
                 let h23 = get_height t23 in
                 let h4 = get_height t4 in
                 let newH' = 1 + (max h23 h4) in
                 let newH = 1 + (max h1 newH') in
                 Tree(z, newH, t1, Tree(x, newH', t23, t4))
 | _ -> failwith "Error, unexpected tree configuration for rotate right"

let rotate_left_right = function
   Tree(c, hc, Tree(a, ha, al, Tree(b, hb, bl, br)), cr) ->
      let ha' = 1 + (max (get_height al) (get_height br)) in
      let hc' = 1 + (max (get_height bl) (get_height cr)) in
      let hb' = 1 + (max ha' hc') in
      Tree(b, hb', Tree(a, ha', al, br), Tree(c, hc', bl, cr))
 | _ -> failwith "Error, unexpected tree configuration for rotate left right"

let rotate_right_left = function
   Tree(a, _, al, Tree(c, _, Tree(b, _, bl, br), cr)) ->
      let ha = 1 + (max (get_height al) (get_height bl)) in
      let hc = 1 + (max (get_height br) (get_height cr)) in
      let hb = 1 + (max ha hc) in
      Tree(b, hb, Tree(a, ha, al, bl), Tree(c, hc, br, cr))
 | _ -> failwith "Error, unexpected tree configuration for rotate right left"

let rebalance tree =
   let balance = get_balance tree in
   if balance < -1 then
      match tree with
         Tree(_, _, left, _) ->
            let bl = get_balance left in
            if bl <= 0 then
               rotate_right tree
            else
               rotate_left_right tree
      | _ -> failwith "Invalid tree shape"
   else if 1 < balance then
      match tree with
         Tree(_, _, _, right) ->
            let br = get_balance right in
            if br >= 0 then
               rotate_left tree
            else
               rotate_right_left tree
      | _ -> failwith "Invalid tree shape"
   else tree

let rec insert tree x = match tree with
   Empty -> new_tree x
 | Tree(e, _, left, right) ->
       let newNode =
          if x = e then tree
          else if x < e then
             let newLeft = insert left x in
             let newHl = get_height newLeft in
             let hr = get_height right in
             Tree(e, 1 + (max newHl hr), newLeft, right)
          else
             let newRight = insert right x in
             let newHr = get_height newRight in
             let hl = get_height left in
             Tree(e, 1 + (max hl newHr), left, newRight)
       in
       if is_unbalanced newNode then
          rebalance newNode
       else newNode

(* insert function for unbalanced binary search tree, useful for testing
 * the rebalance function *)
let rec dumb_insert tree x = match tree with
   Empty -> new_tree x
 | Tree(e, h, left, right) ->
       if e = x then tree
       else if e < x then
          let newRight = dumb_insert right x in
          let newHeight = 1 + (max (get_height left) (get_height newRight)) in
          Tree(e, newHeight, left, newRight)
       else
          let newLeft = dumb_insert left x in
          let newHeight = 1 + (max (get_height newLeft) (get_height right)) in
          Tree(e, newHeight, newLeft, right)

let make_dumb_tree list = List.fold_left dumb_insert Empty list

let rec extractMin tree = match tree with
   Empty -> failwith "Error, called extractMin on empty tree"
 | Tree(e, h, Empty, right) -> (right, e)
 | Tree(e, h, left, right) ->
       let (newLeft, min) = extractMin left in
       let newHeight = 1 + (max (get_height newLeft) (get_height right)) in
       let newNode = Tree(e, newHeight, newLeft, right) in
       if is_unbalanced newNode then
          (rebalance newNode, min)
       else
          (newNode, min)

let rec delete tree x = match tree with
   Empty -> tree
 | Tree(e, h, left, right) ->
       let newNode =
          if e = x then
             match right with
               Empty -> left
             | _ ->
                let (newRight, min) = extractMin right in
                let newHeight = 1 + (max (get_height left) (get_height newRight)) in
                Tree(min, newHeight, left, newRight)
          else if e < x then
             let newRight = delete right x in
             let newHeight = 1 + (max (get_height left) (get_height newRight)) in
             Tree(e, newHeight, left, newRight)
          else
             let newLeft = delete left x in
             let newHeight = 1 + (max (get_height newLeft) (get_height right)) in
             Tree(e, newHeight, newLeft, right)
       in
       if is_unbalanced newNode then
          rebalance newNode
       else newNode

let make_tree list = List.fold_left insert Empty list

let single_insert =
  match make_tree [1] with
    Tree(1, 1, Empty, Empty) -> ()
  | _ -> failwith "Error: single insert failed"

let double_insert =
  match make_tree [1; 2] with
    Tree(1, 2, Empty, Tree(2, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: double insert failed"

let insert_left_rotation =
  match make_tree [1; 2; 3] with
    Tree(2, 2, Tree(1, 1, Empty, Empty),
               Tree(3, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: insert left rotation failed"

let insert_right_rotation =
  match make_tree [3; 2; 1] with
    Tree(2, 2, Tree(1, 1, Empty, Empty),
               Tree(3, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: insert right rotation failed"

let insert_left_right_rotation =
  match make_tree [3; 1; 2] with
    Tree(2, 2, Tree(1, 1, Empty, Empty),
               Tree(3, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: insert left right rotation failed"

let insert_right_left_rotation =
  match make_tree [1; 3; 2] with
    Tree(2, 2, Tree(1, 1, Empty, Empty),
               Tree(3, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: insert right left rotation failed"

let insert_deep_left_rotation =
  match make_tree [1; 2; 3; 4; 5] with
    Tree(2, 3, Tree(1, 1, Empty, Empty),
               Tree(4, 2, Tree(3, 1, Empty, Empty),
                          Tree(5, 1, Empty, Empty))) -> ()
  | _ -> failwith "Error: insert deep left rotation failed"

let insert_deep_right_left_rotation =
  match make_tree [1; 2; 3; 5; 4] with
    Tree(2, 3, Tree(1, 1, Empty, Empty),
               Tree(4, 2, Tree(3, 1, Empty, Empty),
                          Tree(5, 1, Empty, Empty))) -> ()
  | _ -> failwith "Error: insert deep right left rotation failed"

let insert_deep_right_rotation =
  match make_tree [5; 4; 3; 2; 1] with
    Tree(4, 3, Tree(2, 2, Tree(1, 1, Empty, Empty),
                          Tree(3, 1, Empty, Empty)),
               Tree(5, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: insert deep right rotation failed"

let insert_deep_left_right_rotation =
  match make_tree [5; 4; 3; 1; 2] with
    Tree(4, 3, Tree(2, 2, Tree(1, 1, Empty, Empty),
                          Tree(3, 1, Empty, Empty)),
               Tree(5, 1, Empty, Empty)) -> ()
  | _ -> failwith "Error: insert deep right rotation failed"


