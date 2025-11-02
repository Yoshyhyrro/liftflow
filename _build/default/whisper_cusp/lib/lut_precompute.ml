(* lut_precompute.ml
   - relabel LUT と簡易の p-adic / Witt 補助関数（スタブ）
   - 実装は最小可動版で、後で詳細化してください
*)

type relabel_lut = {
  height_to_p_adic: int array;    (* height -> p-adic representative (stub: int) *)
  goppa_cluster: int array;       (* height -> goppa cluster id *)
  ariki_koike_cell: int array;    (* height -> ak cell id *)
}

let make_sample_lut ~max_height =
  { height_to_p_adic = Array.init (max_height + 1) (fun i -> i);
    goppa_cluster = Array.init (max_height + 1) (fun i -> i mod 4);
    ariki_koike_cell = Array.init (max_height + 1) (fun i -> i mod 8);
  }

(* stub: p-adic rep is modelled as a single integer for now *)
let compute_p_adic_from_height (h:int) : int = h

(* stub: decompose into Witt coordinates (here floats for demo) *)
let decompose_witt (p_adic:int) : float array = [| float_of_int p_adic |]
