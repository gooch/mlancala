open Mancala

let () =
  let board = init_board () in
  (* Here you can add more logic to handle user input and game loop *)
  print_endline "Welcome to Mancala!\n";
  print_board board;
  game_loop board
;;
