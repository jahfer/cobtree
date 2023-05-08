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

let suite = "Density tests" >::: [
  "range_of" >:: test_range_of;
  "ratios" >:: test_ratios;
]