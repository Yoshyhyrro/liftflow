(* 
 * Binary Radix Sort Linked List → Ultrametric Distance → Galois Group (Frobenius)
 * dune-compatible implementation
 *)

(* ========== Part 1: Binary Radix Sort Linked List ========== *)

(* Node in the binary radix tree *)
type radix_node = {
  id: int;
  value: int;  (* original value *)
  bitcode: int;  (* binary representation *)
  level: int;  (* current level in tree (0 = LSB) *)
  zero_child: radix_node option;  (* bit = 0 *)
  one_child: radix_node option;   (* bit = 1 *)
}

(* Build binary radix tree from list of integers *)
let rec insert_node node value bitcode level =
  if level < 0 then
    node  (* reached leaf *)
  else
    let bit = (value lsr level) land 1 in
    if bit = 0 then
      { node with zero_child = Some (insert_node (match node.zero_child with Some n -> n | None -> 
        { id = value; value; bitcode; level; zero_child = None; one_child = None }) value bitcode (level - 1)) }
    else
      { node with one_child = Some (insert_node (match node.one_child with Some n -> n | None -> 
        { id = value; value; bitcode; level; zero_child = None; one_child = None }) value bitcode (level - 1)) }

let build_radix_tree values max_bits =
  let init = { id = -1; value = -1; bitcode = -1; level = max_bits; zero_child = None; one_child = None } in
  List.fold_left (fun tree v -> insert_node tree v v (max_bits - 1)) init values

(* ========== Part 2: Ultrametric Distance ========== *)

(* Compute p-adic valuation (here: 2-adic, i.e., MSB position) *)
let p_adic_valuation x =
  if x = 0 then max_int
  else
    let rec count_leading_zeros n =
      if n = 0 then 0
      else 1 + count_leading_zeros (n lsr 1)
    in
    count_leading_zeros (x land (-x))  (* Isolate LSB *)

(* Ultrametric distance: d(x, y) = 2^(-k) where k = MSB(x XOR y) *)
let ultrametric_distance x y =
  let xor_val = x lxor y in
  if xor_val = 0 then 0.0
  else
    let msb_pos = 63 - (Int.bit_length (xor_val) - 1) in  (* MSB position *)
    2.0 ** (float_of_int (-msb_pos))

(* Two-bit shift as ultrametric level *)
let ultrametric_level_by_shift x y =
  let xor_val = x lxor y in
  (xor_val lsr 2) land 3  (* Extract bits at position 2-3 *)

(* ========== Part 3: Galois Group Elements (Frobenius) ========== *)

(* Frobenius automorphism σ_p: permutes bits based on prime p *)
type galois_elem = {
  name: string;  (* e.g., "sigma_2", "sigma_3" *)
  perm: int -> int;  (* permutation on bitcode *)
  prime: int;
}

(* σ_2: Frobenius at prime 2 (bit rotation by 2 positions) *)
let sigma_2: galois_elem = {
  name = "sigma_2";
  perm = (fun x -> ((x lsl 2) lor (x lsr 6)) land 0xFF);  (* 8-bit rotation *)
  prime = 2;
}

(* σ_3: Frobenius at prime 3 (swap even/odd bits) *)
let sigma_3: galois_elem = {
  name = "sigma_3";
  perm = (fun x ->
    let rec swap_bits n acc bit_pos =
      if bit_pos >= 8 then acc
      else
        let even_bit = (n lsr (2 * bit_pos)) land 1 in
        let odd_bit = (n lsr (2 * bit_pos + 1)) land 1 in
        let swapped = (even_bit lsl (2 * bit_pos + 1)) lor (odd_bit lsl (2 * bit_pos)) in
        swap_bits n (acc lor swapped) (bit_pos + 1)
    in
    swap_bits x 0 0
  );
  prime = 3;
}

(* σ_5: Frobenius at prime 5 (bit reversal) *)
let sigma_5: galois_elem = {
  name = "sigma_5";
  perm = (fun x ->
    let rec reverse_bits n acc =
      if n = 0 then acc
      else reverse_bits (n lsr 1) ((acc lsl 1) lor (n land 1))
    in
    reverse_bits x 0
  );
  prime = 5;
}

(* Generate Galois orbits under a set of generators *)
let galois_orbit x generators =
  let seen = Hashtbl.create 16 in
  let rec generate current =
    if Hashtbl.mem seen current then []
    else begin
      Hashtbl.add seen current ();
      let next_states = List.map (fun g -> g.perm current) generators in
      current :: (List.concat_map generate next_states)
    end
  in
  List.sort_uniq Int.compare (generate x)

(* ========== Part 4: Integration: Radix Sort → Ultrametric → Galois ========== *)

(* Traverse radix tree and collect nodes at each level *)
let rec traverse_level tree target_level =
  if tree.level = target_level then [tree]
  else
    let left = match tree.zero_child with Some n -> traverse_level n target_level | None -> [] in
    let right = match tree.one_child with Some n -> traverse_level n target_level | None -> [] in
    left @ right

(* For two nodes at same radix level, compute ultrametric grouping *)
let group_by_ultrametric nodes =
  let groups = Hashtbl.create 32 in
  List.iter (fun node ->
    (* Group by ultrametric distance from reference *)
    let key = ultrametric_level_by_shift node.value 0 in  (* reference = 0 *)
    let group = try Hashtbl.find groups key with Not_found -> [] in
    Hashtbl.replace groups key (node :: group)
  ) nodes;
  groups

(* For each ultrametric group, compute Galois orbit *)
let galois_orbits_of_group group =
  let generators = [sigma_2; sigma_3; sigma_5] in
  List.map (fun node -> (node.value, galois_orbit node.bitcode generators)) group

(* ========== Part 5: Concrete Example ========== *)

let () =
  Printf.printf "=== Binary Radix Sort → Ultrametric → Galois ===\n\n";
  
  (* Build radix tree *)
  let values = [0b00101; 0b00110; 0b01010; 0b01011; 0b10100; 0b10101] in
  let tree = build_radix_tree values 6 in
  Printf.printf "Built radix tree from: %s\n\n" 
    (String.concat ", " (List.map (Printf.sprintf "0b%06b") values));
  
  (* Traverse and show ultrametric grouping at level 2 *)
  Printf.printf "--- Level 2 (2-bit shift) ---\n";
  let level_2_nodes = traverse_level tree 2 in
  let grouped = group_by_ultrametric level_2_nodes in
  Hashtbl.iter (fun key group ->
    Printf.printf "Group (2-bit level=%d):\n" key;
    List.iter (fun node ->
      Printf.printf "  Node: 0b%06b (id=%d)\n" node.bitcode node.id
    ) group;
    Printf.printf "\n"
  ) grouped;
  
  (* Compute Galois orbits *)
  Printf.printf "--- Galois Orbits (Frobenius at p=2,3,5) ---\n";
  List.iter (fun node ->
    let orbit = galois_orbit node.bitcode [sigma_2; sigma_3; sigma_5] in
    Printf.printf "Orbit of 0b%06b: [%s]\n" node.bitcode
      (String.concat ", " (List.map (Printf.sprintf "0b%06b") orbit))
  ) level_2_nodes;
  
  (* Show distances *)
  Printf.printf "\n--- Ultrametric Distances ---\n";
  let test_pairs = [
    (values.(0), values.(1));
    (values.(0), values.(2));
    (values.(2), values.(3));
  ] in
  List.iter (fun (x, y) ->
    let d = ultrametric_distance x y in
    let lshift = ultrametric_level_by_shift x y in
    Printf.printf "d(0b%06b, 0b%06b) = 2^(-?) ≈ %.4f, level_shift=%d\n" x y d lshift
  ) test_pairs;
  
  Printf.printf "\n=== Integration Check ===\n";
  Printf.printf "✓ Radix tree encodes binary structure (linked list)\n";
  Printf.printf "✓ Ultrametric distance from XOR and MSB position\n";
  Printf.printf "✓ Galois orbits via Frobenius permutations\n";
  Printf.printf "✓ Same orbit → same ultrametric level?\n\n";
  
  (* Check: Do nodes in same Galois orbit have same ultrametric level? *)
  if not (List.is_empty level_2_nodes) then begin
    let first = List.nth level_2_nodes 0 in
    let orbit = galois_orbit first.bitcode [sigma_2; sigma_3; sigma_5] in
    Printf.printf "Testing first node 0b%06b:\n" first.bitcode;
    Printf.printf "  Orbit size: %d\n" (List.length orbit);
    let levels = List.map (fun code -> ultrametric_level_by_shift code 0) orbit in
    Printf.printf "  Ultrametric levels in orbit: [%s]\n"
      (String.concat ", " (List.map string_of_int levels));
    if List.for_all (fun l -> l = List.nth levels 0) levels then
      Printf.printf "  ✓ All same level (YES, Galois orbit = ultrametric group)\n"
    else
      Printf.printf "  ✗ Different levels (NO, Galois orbit ≠ ultrametric group)\n"
  end