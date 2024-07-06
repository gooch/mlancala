open Mancala

let () =
  let board = init_board () in
  (* Here you can add more logic to handle user input and game loop *)
  print_endline "Welcome to Mancala!\n";
  game_loop board Player1

