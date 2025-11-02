(* preflow_push_lifting.ml
   - Preflow-Push の簡易雛形と、lifting を行う関数群（スタブ・最小実装）
   - 実際のネットワーク／辺情報は省略し、Linked_list ノード同士を "隣接" とみなす
*)

open Linked_list
open Lut_precompute
open Port_scheduler

type port_info = {
  port_assignment: int;
  estimated_delay: float;
}

let update_flow _u _v _delta = ()
let capacity_u_v _u _v = 100
let residual_capacity _u _v = 100

let get_neighbors node : binary_node list =
  let acc = ref [] in
  (match node.left with Some n -> acc := n :: !acc | None -> ());
  (match node.right with Some n -> acc := n :: !acc | None -> ());
  List.rev !acc

let push_with_lifting (u:binary_node) (v:binary_node) (lut:relabel_lut) (port_scheduler:unit) : port_info option =
  if u.excess > 0 then begin
    let delta = min u.excess (capacity_u_v u v) in
    u.excess <- u.excess - delta;
    v.excess <- v.excess + delta;
    update_flow u v delta;

    let v_height_old = v.height in
    (* LUT から新しい "height"(=p-adic rep) を決める *)
    let new_height =
      if v_height_old < Array.length lut.height_to_p_adic then
        lut.height_to_p_adic.(v_height_old)
      else v_height_old
    in
    let p_adic_val = compute_p_adic_from_height new_height in
    v.p_adic_coords <- decompose_witt p_adic_val;

    let goppa_cluster = lut.goppa_cluster.(new_height mod (Array.length lut.goppa_cluster)) in
    let ak_cell = lut.ariki_koike_cell.(new_height mod (Array.length lut.ariki_koike_cell)) in
    let port_assignment = decide_port goppa_cluster ak_cell in
    assign_port v port_assignment;

    Some { port_assignment; estimated_delay = float_of_int ak_cell *. 0.5 }
  end else None

let relabel_with_lut (v:binary_node) (lut:relabel_lut) : int =
  let old = v.height in
  let new_height =
    if old < Array.length lut.height_to_p_adic then
      lut.height_to_p_adic.(old)
    else old
  in
  let p_adic_val = compute_p_adic_from_height new_height in
  v.p_adic_coords <- decompose_witt p_adic_val;
  v.height <- new_height;
  new_height

let preflow_push_with_lifting (root:binary_node) (lut:relabel_lut) (port_scheduler:unit) : unit =
  (* 簡易キュー: ノード列 *)
  let queue = ref [root] in
  while !queue <> [] do
    let u = List.hd !queue in
    queue := List.tl !queue;
    let pushed = ref false in

    let neighbors = get_neighbors u in
    List.iter (fun v ->
      match push_with_lifting u v lut port_scheduler with
      | Some _ -> pushed := true; queue := !queue @ [v]
      | None -> ()
    ) neighbors;

    if not !pushed then begin
      let old_h = u.height in
      let new_h = relabel_with_lut u lut in
      if new_h > old_h + 1 then
        Printf.printf "CUSP detected at node %d: height jump %d -> %d\n" u.value old_h new_h;
      queue := !queue @ [u]
    end
  done
