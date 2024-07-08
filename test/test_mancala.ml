open OUnit2
open Mancala

let test_player_pits _ =
  let board = init_board () in
  let p : pit array = Array.make 6 4 in
  assert_equal (player_pits Player1 board) p
;;

let test_player_1_move _ =
  let board = init_board () in
  let p : pit array = [| 0; 5; 5; 5; 5; 4 |] in
  let new_board = sow_seeds board 0 in
  assert_equal (player_pits Player1 new_board) p
;;

let test_player_2_move _ =
  let board = change_player (init_board ()) in
  let p : pit array = [| 0; 5; 5; 5; 5; 4 |] in
  let new_board = sow_seeds board 0 in
  assert_equal (player_pits Player2 new_board) p
;;

let test_player_1_acc _ =
  let board = init_board () in
  let new_board = sow_seeds board 2 in
  assert_equal (player_store Player1 new_board) 1
;;

let test_player_1_passes_op_store _ =
  let board = init_board () in
  board.pits.(5) <- 10;
  let new_board = sow_seeds board 5 in
  assert_equal (player_store Player1 new_board) 1;
  assert_equal (player_store Player2 new_board) 0
;;

let test_player_2_acc _ =
  let board = change_player (init_board ()) in
  let new_board = sow_seeds board 4 in
  assert_equal (player_store Player2 new_board) 1
;;

let test_opposite_pit _ = assert_equal (opposite_pit 11) 1

let test_player_1_capture _ =
  let board = init_board () in
  board.pits.(5) <- 0;
  let new_board = sow_seeds board 1 in
  assert_equal (player_store Player1 new_board) 5
;;

let test_player_2_capture _ =
  let board = change_player (init_board ()) in
  board.pits.(11) <- 0;
  let new_board = sow_seeds board 0 in
  assert_equal (player_store Player2 new_board) 5
;;

let test_second_turn _ =
  let board = init_board () in
  let new_board = sow_seeds board 2 in
  assert_equal new_board.player_turn Player1
;;

let test_second_turn_capture _ =
  let board = init_board () in
  let new_board = change_player board in
  board.pits.(11) <- 0;
  let new_board = sow_seeds new_board 0 in
  assert_equal new_board.player_turn Player2
;;

let suite =
  "Mancala Tests"
  >::: [ "test_player_pits" >:: test_player_pits
       ; "test_player_1_move" >:: test_player_1_move
       ; "test_player_2_move" >:: test_player_2_move
       ; "test_player_1_acc" >:: test_player_1_acc
       ; "test_player_1_passes_op_store" >:: test_player_1_passes_op_store
       ; "test_player_2_acc" >:: test_player_2_acc
       ; "test_opposite_pit" >:: test_opposite_pit
       ; "test_player_1_capture" >:: test_player_1_capture
       ; "test_player_2_capture" >:: test_player_2_capture
       ; "test_second_turn" >:: test_second_turn
       ; "test_second_turn_capture" >:: test_second_turn_capture
       ]
;;

let () = run_test_tt_main suite
