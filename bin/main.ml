open Mancala

let () =
  let board = init_board () in
  print_endline "Welcome to Mancala!\n";
  print_board board;
  game_loop board
;;
