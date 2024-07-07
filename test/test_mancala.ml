open OUnit2
open Mancala

let test_player_pits _ =
  let board = init_board () in
    let p : pit array = Array.make 6 4 in
  assert_equal (player_pits Player1 board) p

let test_player_1_move _ =
  let board = init_board () in
    let p : pit array = [| 0; 5; 5; 5; 5; 4 |] in
      let new_board = sow_seeds board Player1 0 in
        assert_equal (player_pits Player1 new_board) p

let test_player_2_move _ =
  let board = init_board () in
    let p : pit array = [| 0; 5; 5; 5; 5; 4 |] in
      let new_board = sow_seeds board Player2 0 in
        assert_equal (player_pits Player2 new_board) p

let test_player_1_acc _ = 
  let board = init_board () in
    let new_board = sow_seeds board Player1 2 in
      assert_equal (player_store Player1 new_board) 1

let suite =
  "Mancala Tests" >::: [
    "test_player_pits" >:: test_player_pits;
    "test_player_1_move" >:: test_player_1_move;
    "test_player_2_move" >:: test_player_2_move;
    "test_player_1_acc" >:: test_player_1_acc;
   ]
;;

let () = run_test_tt_main suite
