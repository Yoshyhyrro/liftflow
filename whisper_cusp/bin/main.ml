(* main.ml: 簡易デモで preflow_push_with_lifting を呼ぶ *)

open Linked_list
open Lut_precompute
open Preflow_push_lifting

let () =
  (* サンプルノードを作る *)
  let n1 = make_node 1 in
  let n2 = make_node 2 in
  let n3 = make_node 3 in
  (* 木構造を作る *)
  set_children n1 ~left:(Some n2) ~right:(Some n3);

  (* 初期値: n1 に excess を与える *)
  n1.excess <- 10;

  let lut = make_sample_lut ~max_height:16 in
  let scheduler = () in

  Printf.printf "Starting preflow_push_with_lifting demo...\n";
  preflow_push_with_lifting n1 lut scheduler;
  Printf.printf "Done. n1.excess=%d n2.excess=%d n3.excess=%d\n" n1.excess n2.excess n3.excess
