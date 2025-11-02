(* port_scheduler.ml
   - 簡易のポート割り当てロジック（stub）
   - 実際のスケジューラはここを拡張してください
*)

open Linked_list

let decide_port (goppa_cluster:int) (ak_cell:int) : int =
  (* stub: シンプルな合成関数でポートを決める *)
  (goppa_cluster + ak_cell) mod 8

let assign_port (_node: binary_node) (_port:int) : unit =
  (* 実環境ではノードにポートメタデータを書き込む等の処理を行う *)
  ()

let create_scheduler () = ()
