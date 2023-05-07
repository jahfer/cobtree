open Kcas
module Z_Int = Z
module Z_Rational = Q

module Math_util = struct
  let log2 x = (Float.of_int x)
    |> Float.log2
    |> Float.ceil
    |> Int.of_float

  let next_power_of_two n =
    if n <= 1 then
      1
    else
      let log2_n = log2 (n - 1) in
      Int.shift_left 1 (log2_n + 1)

  let one_over n =
    Z_Rational.make Z_Int.one (Z_Int.of_int n)
end

module Density = struct
  type t = {
    max_n : int;
    range : (Z_Rational.t * Z_Rational.t);
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
    Z_Rational.(n >= min_d && n <= max_d)

  let range_of n =
    let num_densities = Math_util.log2 n in

    (* max density for 2^num_densities cells: 1/2 *)
    let t_min = Math_util.one_over 2 in
    (* max density for 2^1 cells: 1 *)
    let t_max = Z_Rational.one in
    (* min density for 2^num_densities cells: 1/4 *)
    let p_max = Math_util.one_over 4 in
    (* min density for 2^1 cells: 1/8 *)
    let p_min = Math_util.one_over 8 in

    let t_delta = Z_Rational.(t_max - t_min) in
    let p_delta = Z_Rational.(p_max - p_min) in

    let ratio x =
      let open Z_Int in
      Z_Rational.make ((of_int x) - one) ((of_int num_densities) - one) in

    let make_density x =
      let r = ratio x in
      {
        max_n = Int.shift_left 1 x;
        range = (
          Z_Rational.(p_min + r * p_delta),
          Z_Rational.(t_max - r * t_delta)
        )
      } in

    List.init num_densities (fun x -> make_density (x + 1))
end

module Packed_memory_array = struct
  type cfg = {
    density_scale : Density.t list;
  }

  type 'a t = {
    cells : 'a Loc.t array;
    active_idx_range : (int * int);
    config : cfg;
  }

  let allocation_size (num_keys : int) =
    let t_min = 0.5 in
    let p_max = 0.25 in
    let ideal_density = (t_min -. p_max) /. 2. in

    let length = (float_of_int num_keys) /. ideal_density in

    (* To get a balanced tree, we need to find the
       closest double-exponential number (x = 2^2^i) *)
    let length_norm = (Float.log2 length)
      |> ceil
      |> Int.of_float
      |> Math_util.next_power_of_two
      |> (fun x -> Int.shift_left 2 (x - 1)) in

    length_norm

  let make_cells (len) =
    (* TODO: Loc.make_array *)
    Array.make len (Loc.make 0)

  let make (capacity) =
    let size = allocation_size capacity in
    let cells = make_cells size in

    let left_buffer_space = Int.shift_right size 2 in
    let start_idx = left_buffer_space in
    let end_idx = size - left_buffer_space in

    let config = {
      density_scale = Density.range_of size;
    } in

    {
      cells;
      active_idx_range = (start_idx, end_idx);
      config;
    }
end