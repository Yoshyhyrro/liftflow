(* linked_list.ml
   - Binary linked list node + traversal helpers
   - シンプルな二分 (0/1) 子を持つノードで表現
*)

type binary_node = {
  mutable value: int;            (* binary value (for identification) *)
  mutable excess: int;           (* preflow excess *)
  mutable height: int;           (* distance label *)
  mutable p_adic_coords: float array; (* Witt coordinates (stub as floats) *)
  mutable left: binary_node option;   (* 0-child *)
  mutable right: binary_node option;  (* 1-child *)
}

let make_node v =
  { value = v;
    excess = 0;
    height = 0;
    p_adic_coords = [| 0.0 |];
    left = None;
    right = None;
  }

let rec traverse_inorder root =
  let acc = ref [] in
  let rec aux = function
    | None -> ()
    | Some n ->
      aux n.left;
      acc := !acc @ [n];
      aux n.right
  in
  aux (Some root);
  !acc

let set_children node ~left ~right =
  node.left <- left;
  node.right <- right
