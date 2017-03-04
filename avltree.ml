
type height = int
type 'a avl_tree = Empty
                 | Tree of 'a * height * 'a avl_tree * 'a avl_tree

let get_height = function
   Empty -> 0
 | Tree(_, h, _, _) -> h

let get_balance = function
   Empty -> 0
 | Tree (_, _, l, r) -> -(get_height l) + (get_height r)

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

 (*
let rec insert' tree x = match tree with
   Empty -> new_tree x
 | Tree(e, _, left, right) ->
       if x = e then tree
       else if x < e then
          match left with
             Empty ->
                Tree(e, _, new_tree x, right)
       else
          *)

let rec insert tree x = match tree with
   Empty -> new_tree x
 | Tree(e, _, Empty, Empty) as node ->
       if x = e then node
       else if x < e then Tree(e, 2, new_tree x, Empty)
       else Tree(e, 2, Empty, new_tree x)
 | Tree(e, b, Empty, (Tree(r, _, Empty, Empty) as right)) as node ->
       if x = e then node
       else if x < e then Tree(e, 2, new_tree x, right)
       else if r < x then Tree(r, 2, new_tree e, new_tree x)
       else Tree(x, 2, new_tree e, new_tree r)
 | Tree(e, b,
        (Tree(l, _, Empty, Empty) as left),
        Empty) as node ->
       if x = e then node
       else if e < x then Tree(e, 2, left, new_tree x)
       else if x < l then Tree(l, 2, new_tree x, new_tree e)
       else Tree(x, 2, new_tree l, new_tree e)
 | Tree(e, _,
        (Tree(l, hl, _, _) as left),
        (Tree(r, hr, _, _) as right)) as node ->
       if x = e || x = l || x = r then node
       else if x < e then
          let newLeft = insert left x in
          let newHl = get_height newLeft in
          let newNode = Tree(e, 1 + (max newHl hr), newLeft, right) in
          let newB = get_balance newNode in
          if newB < -1 then
             if x < l then
                rotate_right newNode
             else
                rotate_left_right newNode
          else newNode
       else
          let newRight = insert right x in
          let newHr = get_height newRight in
          let newNode = Tree(e, 1 + (max hl newHr), left, newRight) in
          let newB = get_balance newNode in
          if newB > 1 then
             if x < r then
                rotate_right_left newNode
             else
                rotate_left newNode
          else newNode
 | _ -> failwith "Error, unexpected tree configuration during insert"

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


