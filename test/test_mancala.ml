open OUnit2
open Mancala

(* Helper function to create a board for testing *)
let create_board p1_pits p2_pits p1_store p2_store = 
  { player1_pits = Array.of_list p1_pits; 
    player2_pits = Array.of_list p2_pits; 
    player1_store = p1_store; 
    player2_store = p2_store 
  }

(* Test initialization of the board *)
let test_init_board _ =
  let board = init_board () in
  assert_equal (Array.to_list board.player1_pits) [4; 4; 4; 4; 4; 4];
  assert_equal (Array.to_list board.player2_pits) [4; 4; 4; 4; 4; 4];
  assert_equal board.player1_store 0;
  assert_equal board.player2_store 0

(* Test sowing seeds *)
let test_sow_seeds _ =
  let board = create_board [4; 4; 4; 4; 4; 4] [4; 4; 4; 4; 4; 4] 0 0 in
  let new_board = sow_seeds board Player1 0 in
  assert_equal (Array.to_list new_board.player1_pits) [0; 5; 5; 5; 5; 4];
  assert_equal (Array.to_list new_board.player2_pits) [4; 4; 4; 4; 4; 4];
  assert_equal new_board.player1_store 1;
  assert_equal new_board.player2_store 0

let suite =
  "Mancala Tests" >::: [
    "test_init_board" >:: test_init_board;
    "test_sow_seeds" >:: test_sow_seeds;
  ]

let () =
  run_test_tt_main suite

