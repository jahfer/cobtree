open OUnit2

let suite = "Packed Memory Array" >::: [
  Packed_memory_array_test.suite;
  Density_test.suite;
]

let _ = run_test_tt_main suite