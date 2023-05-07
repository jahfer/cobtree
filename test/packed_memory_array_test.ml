open OUnit2
open Cobtree

let test_foo _ =
  let pma = Packed_memory_array.make (3) in
  assert_equal 256 (Array.length pma.cells) ~printer:string_of_int

let suite = "Packed memory array tests" >::: [
  "foo" >:: test_foo;
]