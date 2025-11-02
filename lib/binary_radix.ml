(*
 * Cusp Detection Framework
 * Strategy 1: Ultrametric Discontinuity (Geometric Cusp)
 * Strategy 2: Galois/Frobenius Fixed Points + Orbit Boundaries (Algebraic Cusp)
 * Based on document: 計算的検出戦略：カスプのためのアルゴリズムとシグネチャ
 *)

(* ========== UTILITIES ========== *)

(* Helper: convert int to binary string *)
let int_to_binary ?(width=8) n =
  let rec aux acc n w =
    if w = 0 then acc
    else aux (string_of_int (n land 1) ^ acc) (n lsr 1) (w - 1)
  in
  "0b" ^ aux "" n width

(* bit_length: count significant bits *)
let bit_length n =
  if n = 0 then 0
  else
    let rec aux n len =
      if n = 0 then len
      else aux (n lsr 1) (len + 1)
    in
    aux n 0

(* ========== STRATEGY 1: ULTRAMETRIC DISCONTINUITY ========== *)

module Strategy1_Ultrametric = struct
  
  (* Compute ultrametric distance: d(x,y) = 2^(-k) where k = MSB(x XOR y) *)
  let ultrametric_distance x y =
    let xor_val = x lxor y in
    if xor_val = 0 then (0, 0.0)  (* Return (msb_pos, distance) *)
    else
      let msb_pos = bit_length xor_val - 1 in
      (msb_pos, 2.0 ** (float_of_int (-msb_pos)))
  
  (* Compute distance jumps for sorted sequence *)
  let compute_distance_jumps sorted_values =
    let rec aux values acc =
      match values with
      | [] | [_] -> List.rev acc
      | x :: y :: rest ->
          let (msb_pos, dist) = ultrametric_distance x y in
          aux (y :: rest) ((x, y, msb_pos, dist) :: acc)
    in
    aux sorted_values []
  
  (* Max-Jump algorithm: find non-parametric threshold *)
  (* Input: list of (x, y, msb_pos, distance) tuples *)
  (* Output: Max-Jump threshold and candidate cusps *)
  let max_jump_threshold jumps =
    if List.is_empty jumps then (0, [])
    else
      (* Extract distances and sort *)
      let distances = List.map (fun (_, _, _, d) -> int_of_float (d *. 1e6)) jumps in
      let sorted_dists = List.sort_uniq Int.compare distances in
      
      (* Find max jump between consecutive sorted distances *)
      let rec find_max_jump dists max_jump max_idx idx =
        match dists with
        | [] | [_] -> (max_jump, max_idx)
        | d1 :: d2 :: rest ->
            let jump = d2 - d1 in
            let new_max = if jump > max_jump then jump else max_jump in
            let new_idx = if jump > max_jump then idx else max_idx in
            find_max_jump (d2 :: rest) new_max new_idx (idx + 1)
      in
      
      let (max_jump_val, threshold_idx) = find_max_jump sorted_dists 0 0 0 in
      let threshold = 
        if threshold_idx < List.length sorted_dists then 
          float_of_int (List.nth sorted_dists threshold_idx) /. 1e6
        else 0.0
      in
      
      (* Identify cusps: distances exceeding threshold *)
      let cusps = List.filter (fun (_, _, _, d) -> d >= threshold) jumps in
      (threshold, cusps)
  
  (* Strategy 1: Main detection *)
  let detect sorted_values =
    Printf.printf "=== STRATEGY 1: Ultrametric Discontinuity ===\n";
    let jumps = compute_distance_jumps sorted_values in
    
    Printf.printf "Distance jumps: %d\n" (List.length jumps);
    List.iter (fun (x, y, msb, d) ->
      Printf.printf "  d(%s, %s) = 2^(-%d) ≈ %.6f\n"
        (int_to_binary x) (int_to_binary y) msb d
    ) jumps;
    
    let (threshold, cusps) = max_jump_threshold jumps in
    Printf.printf "\nMax-Jump threshold: %.6f\n" threshold;
    Printf.printf "Cusp candidates (distance >= threshold): %d\n" (List.length cusps);
    List.iter (fun (x, y, msb, d) ->
      Printf.printf "  Cusp at %s → %s (distance: %.6f)\n"
        (int_to_binary x) (int_to_binary y) d
    ) cusps;
    
    cusps
end

(* ========== STRATEGY 2: GALOIS/FROBENIUS FIXED POINTS + ORBIT BOUNDARIES ========== *)

module Strategy2_Galois = struct
  
  (* Frobenius automorphisms *)
  type frobenius = {
    name: string;
    perm: int -> int;
    prime: int;
  }
  
  (* σ_2: bit rotation by 2 *)
  let sigma_2 = {
    name = "σ_2";
    perm = (fun x -> ((x lsl 2) lor (x lsr 6)) land 0xFF);
    prime = 2;
  }
  
  (* σ_3: swap even/odd bits *)
  let sigma_3 = {
    name = "σ_3";
    perm = (fun x ->
      let rec swap_bits n acc bit_pos =
        if bit_pos >= 4 then acc
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
  
  (* σ_5: bit reversal *)
  let sigma_5 = {
    name = "σ_5";
    perm = (fun x ->
      let rec reverse_bits n acc =
        if n = 0 then acc
        else reverse_bits (n lsr 1) ((acc lsl 1) lor (n land 1))
      in
      reverse_bits x 0
    );
    prime = 5;
  }
  
  (* Compute Galois orbit under generators *)
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
  
  (* Compute fixed points: C_σ = {x | σ(x) = x} *)
  let fixed_points sigma values =
    List.filter (fun x -> sigma.perm x = x) values
  
  (* Detect orbit boundaries in tree structure *)
  (* For each value, check if neighbors are in different orbits *)
  let orbit_boundaries sorted_values generators =
    let orbits = List.map (fun v -> (v, galois_orbit v generators)) sorted_values in
    let orbit_map = Hashtbl.create (List.length sorted_values) in
    List.iter (fun (v, orbit) ->
      Hashtbl.replace orbit_map v (List.hd (List.sort Int.compare orbit))  (* canonical rep *)
    ) orbits;
    
    (* Find boundaries: adjacent values in different orbits *)
    let rec find_boundaries values acc =
      match values with
      | [] | [_] -> List.rev acc
      | v1 :: v2 :: rest ->
          let orbit1 = Hashtbl.find orbit_map v1 in
          let orbit2 = Hashtbl.find orbit_map v2 in
          if orbit1 <> orbit2 then
            find_boundaries (v2 :: rest) ((v1, v2, orbit1, orbit2) :: acc)
          else
            find_boundaries (v2 :: rest) acc
    in
    find_boundaries sorted_values []
  
  (* Combine: fixed points that are orbit boundaries = cusps *)
  let detect sorted_values =
    Printf.printf "\n=== STRATEGY 2: Galois/Frobenius Fixed Points ===\n";
    
    let generators = [sigma_2; sigma_3; sigma_5] in
    
    (* Compute fixed points for each frobenius *)
    Printf.printf "Fixed points:\n";
    List.iter (fun sigma ->
      let fixed = fixed_points sigma sorted_values in
      Printf.printf "  %s: {%s}\n" sigma.name
        (String.concat ", " (List.map int_to_binary fixed))
    ) generators;
    
    (* Compute orbit boundaries *)
    Printf.printf "\nOrbit boundaries (adjacent values in different orbits):\n";
    let boundaries = orbit_boundaries sorted_values generators in
    List.iter (fun (v1, v2, o1, o2) ->
      Printf.printf "  %s [orbit %d] → %s [orbit %d]\n"
        (int_to_binary v1) o1 (int_to_binary v2) o2
    ) boundaries;
    
    (* Cusp candidates: nodes that are both fixed points AND orbit boundaries *)
    Printf.printf "\nCusp candidates (fixed + boundary):\n";
    let all_fixed = 
      List.concat (List.map (fun sigma -> fixed_points sigma sorted_values) generators)
      |> List.sort_uniq Int.compare
    in
    
    let cusps = List.filter (fun (v1, v2, _, _) ->
      List.mem v1 all_fixed || List.mem v2 all_fixed
    ) boundaries in
    
    Printf.printf "  Found %d cusp candidates\n" (List.length cusps);
    List.iter (fun (v1, v2, _, _) ->
      Printf.printf "    Cusp: %s ↔ %s\n" (int_to_binary v1) (int_to_binary v2)
    ) cusps;
    
    cusps
end

(* ========== UNIFIED CUSP DETECTION ========== *)

module CuspDetection = struct
  
  type cusp_signature = {
    ultrametric_cusps: (int * int * int * float) list;  (* (x, y, msb, dist) *)
    galois_cusps: (int * int * int * int) list;         (* (v1, v2, o1, o2) *)
  }
  
  let detect_cusps sorted_values =
    let s1 = Strategy1_Ultrametric.detect sorted_values in
    let s2 = Strategy2_Galois.detect sorted_values in
    
    Printf.printf "\n=== CUSP INTEGRATION ===\n";
    Printf.printf "Ultrametric cusps: %d\n" (List.length s1);
    Printf.printf "Galois cusps: %d\n" (List.length s2);
    
    {
      ultrametric_cusps = s1;
      galois_cusps = s2;
    }
end

(* ========== MAIN ========== *)

let () =
  Printf.printf "╔════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Cusp Detection: Strategy 1 (Ultrametric) + Strategy 2     ║\n";
  Printf.printf "║  (Galois/Frobenius Fixed Points + Orbit Boundaries)       ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════╝\n\n";
  
  (* Test data: sorted binary values *)
  let test_values = [
    0b00101;  (* 5 *)
    0b00110;  (* 6 *)
    0b01010;  (* 10 *)
    0b01011;  (* 11 *)
    0b10100;  (* 20 *)
    0b10101;  (* 21 *)
  ] in
  
  Printf.printf "Input values (sorted): %s\n\n"
    (String.concat ", " (List.map int_to_binary test_values));
  
  let signature = CuspDetection.detect_cusps test_values in
  
  Printf.printf "\n╔════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  RESULT: Unified Cusp Signature                            ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════╝\n";
  Printf.printf "Total ultrametric cusps: %d\n" (List.length signature.ultrametric_cusps);
  Printf.printf "Total Galois cusps: %d\n" (List.length signature.galois_cusps);
  Printf.printf "\nBoth strategies successfully implemented!\n"