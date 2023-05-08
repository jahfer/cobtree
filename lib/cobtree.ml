open Kcas

module Math_util = struct
  let log2 x = (Float.of_int x)
    |> Float.log2
    |> Float.ceil
    |> Int.of_float

  let next_power_of_two n =
    if n <= 1 then 1
    else
      let log2_n = log2 (n - 1) in
      Int.shift_left 1 (log2_n + 1)
end

module Density = struct
  module type IConfig = sig
    (* max density for 2^num_densities cells: 1/2 *)
    val t_min : Q.t
    (* max density for 2^1 cells: 1 *)
    val t_max : Q.t
    (* min density for 2^num_densities cells: 1/4 *)
    val p_max : Q.t
    (* min density for 2^1 cells: 1/8 *)
    val p_min : Q.t
  end

  type t = {
    max_n : int;
    range : (Q.t * Q.t);
  }

  let compare (a : t) (b : t) =
    let max_cmp = (a.max_n - b.max_n) in
    if max_cmp = 0 then
      let (min_r_a, max_r_a) = a.range in
      let (min_r_b, max_r_b) = b.range in
      let min_range_cmp = Q.compare min_r_a min_r_b in
      if min_range_cmp = 0 then
        Q.compare max_r_a max_r_b
      else
        min_range_cmp
    else
      max_cmp

  let threshold_of_size xs n =
    List.find (fun x -> x.max_n >= n) xs

  let within_threshold n d =
    let (min_d, max_d) = d.range in
    Q.(n >= min_d && n <= max_d)

  let pp_printer f d = let (min_d, max_d) = d.range in
    Format.fprintf f "@[{@ (%a,@ %a)@ when n@ <=@ %d@ }@]@,"
    Q.pp_print min_d Q.pp_print max_d d.max_n

  module Make(M : IConfig) = struct
    let ideal_density = Q.((M.t_min - M.p_max) / (of_int 2))
    let t_delta = Q.(M.t_max - M.t_min)
    let p_delta = Q.(M.p_max - M.p_min)

    let range_list ~max_size =
      let num_densities = Math_util.log2 max_size in
      let ratio x = Q.of_ints (x - 1) (num_densities - 1) in
      let make_density x =
        let r = ratio x in
        {
          max_n = Int.shift_left 1 x;
          range = Q.(
            M.p_min + r * p_delta,
            M.t_max - r * t_delta
          )
        } in

      List.init num_densities @@ fun x -> make_density (x + 1)
  end

  module Default = Make(struct
    open Q
    (* max density for 2^num_densities cells: 1/2 *)
    let t_min = of_ints 1 2
    (* max density for 2^1 cells: 1 *)
    let t_max = one
    (* min density for 2^num_densities cells: 1/4 *)
    let p_max = of_ints 1 4
    (* min density for 2^1 cells: 1/8 *)
    let p_min = of_ints 1 8
  end)
end

module Packed_memory_array = struct
  type cfg = {
    density_scale : Density.t list;
  }

  module D = Density.Default

  type 'a t = {
    cells : 'a Loc.t array;
    active_idx_range : (int * int);
    config : cfg;
  }

  let allocation_size (num_keys : int) =
    let length = Q.((of_int num_keys) / D.ideal_density) in

    (* To get a balanced tree, we need to find the
       closest double-exponential number (x = 2^2^i) *)
    let length_norm = (Float.log2 @@ Q.to_float length)
      |> ceil
      |> Int.of_float
      |> Math_util.next_power_of_two
      |> (fun x -> Int.shift_left 2 (x - 1)) in

    length_norm

  let make_cells (len) =
    (* TODO: Loc.make_array available in 0.4.0 *)
    Array.make len (Loc.make 0)

  let make (capacity) =
    let size = allocation_size capacity in
    let cells = make_cells size in

    let left_buffer_space = Int.shift_right size 2 in
    let start_idx = left_buffer_space in
    let end_idx = size - left_buffer_space in

    let config = {
      density_scale = D.range_list ~max_size:size;
    } in

    {
      cells;
      active_idx_range = (start_idx, end_idx);
      config;
    }
end