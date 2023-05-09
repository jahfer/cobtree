open OUnit2
open Cobtree

module EDensity = struct
  type t = Density.t
  let compare = Density.compare
  let pp_printer = Density.pp_printer
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListDensity = OUnitDiff.ListSimpleMake(EDensity);;

let test_range_of _ =
  let ds = Density.Default.range_list ~max_size:8 in
  assert_equal 3 (List.length ds) ~printer:string_of_int

let test_ratios _ =
  let ds = Density.Default.range_list ~max_size:8 in
  ListDensity.assert_equal ds [
    { max_n = 2; range = (Q.of_ints 1 8, Q.one) };
    { max_n = 4; range = (Q.of_ints 3 16, Q.of_ints 3 4) };
    { max_n = 8; range = (Q.of_ints 1 4, Q.of_ints 1 2) }
  ]

let test_custom_density_config _ =
  let module D = Density.Make(struct
    let p_max = Q.of_ints 1 3
    let t_min = Q.of_ints 2 5

    let p_min = Q.of_ints 1 6
    let t_max = Q.one
  end) in

  let ds = D.range_list ~max_size:8 in
  ListDensity.assert_equal ds [
    { max_n = 2; range = (Q.of_ints 1 6, Q.one) };
    { max_n = 4; range = (Q.of_ints 1 4, Q.of_ints 7 10) };
    { max_n = 8; range = (Q.of_ints 1 3, Q.of_ints 2 5) }
  ]

let test_threshold_of_size _ =
  let printer = fun x ->
    Density.pp_printer Format.str_formatter x;
    Format.flush_str_formatter () in

  let ds = Density.Default.range_list ~max_size:8 in
  assert_equal (Density.threshold_of_size 1 ds) (List.nth ds 0) ~printer;
  assert_equal (Density.threshold_of_size 2 ds) (List.nth ds 0) ~printer;
  assert_equal (Density.threshold_of_size 3 ds) (List.nth ds 1) ~printer;
  assert_equal (Density.threshold_of_size 4 ds) (List.nth ds 1) ~printer;
  assert_equal (Density.threshold_of_size 8 ds) (List.nth ds 2) ~printer

let suite = "Density tests" >::: [
  "range_of" >:: test_range_of;
  "ratios" >:: test_ratios;
  "threshold_of_size" >:: test_threshold_of_size;
  "custom density config" >:: test_custom_density_config;
]