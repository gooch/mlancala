
type pit = int
type store = int
type board = { 
  player1_pits: pit array; 
  player2_pits: pit array; 
  player1_store: store; 
  player2_store: store 
}

type point_of_view = Player1 | Player2


(* Initialize the board with 4 seeds in each pit *)
let init_board () = 
  { player1_pits = Array.make 6 4; 
    player2_pits = Array.make 6 4; 
    player1_store = 0; 
    player2_store = 0 
  }

(* Function to print the current state of the board *)

let print_board board point_of_view =
  Printf.printf "<><><><><><><><><><><><><><><><><><><><><><><><><><><>\n";

  let print_array arr =
    Array.iter (fun x -> Printf.printf "%d " x) arr;
    print_endline ""
  in
  match point_of_view with
  | Player1 ->
    print_endline "Player 2:";
    print_array (Array.of_list (List.rev (Array.to_list board.player2_pits)));
    Printf.printf "Store: %d\n\n" board.player2_store;

    print_endline "Player 1:";
    print_array board.player1_pits;
    Printf.printf "Store: %d\n\n" board.player1_store
  | Player2 ->
    print_endline "Player 1:";
    print_array (Array.of_list (List.rev (Array.to_list board.player1_pits)));
    Printf.printf "Store: %d\n\n" board.player1_store;

    print_endline "Player 2:";
    print_array board.player2_pits;
    Printf.printf "Store: %d\n\n" board.player2_store

(* Function to handle a player's move *)
let sow_seeds board player pit_index =
  let pits, store, opponent_pits, opponent_store = 
    match player with
  | Player1 -> board.player1_pits, board.player1_store, board.player2_pits, board.player2_store 
  | Player2 -> board.player2_pits, board.player2_store, board.player1_pits, board.player1_store 
  in
  let seeds = pits.(pit_index) in
  pits.(pit_index) <- 0;

  let rec distribute_seeds seeds current_index current_pits current_store =
    if seeds = 0 then (current_pits, current_store)
    else
      let next_index = (current_index + 1) mod 14 in
      let next_pits, next_store = 
        if next_index < 6 then 
          (current_pits.(next_index) <- current_pits.(next_index) + 1; current_pits, current_store)
        else if next_index = 6 then 
          (current_pits, current_store + 1)
        else if next_index < 13 then 
          (opponent_pits.(next_index - 7) <- opponent_pits.(next_index - 7) + 1; opponent_pits, opponent_store)
        else 
          (current_pits, current_store)
      in
      distribute_seeds (seeds - 1) next_index next_pits next_store
  in
  let final_pits, final_store = distribute_seeds seeds pit_index pits store in
    match player with
    | Player1 -> { board with player1_pits = final_pits; player1_store = final_store }
    | Player2 -> { board with player2_pits = final_pits; player2_store = final_store }

let rec read_int prompt =
  Printf.printf "%s" prompt;
  match read_line () with
  | exception End_of_file -> 0
  | line -> 
    try int_of_string line 
    with Failure _ -> read_int prompt

let get_move player =
  Printf.printf "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n";
  Printf.printf "Player %d, enter your move (pit index 0-5): " player;
  read_int ""

let other_view point_of_view = 
match point_of_view with
    | Player1 -> Player2
    | Player2 -> Player1



let rec game_loop board point_of_view =
  print_board board point_of_view;

  let pit_index = get_move (match point_of_view with Player1 -> 1 | Player2 -> 2) in
    let new_board = sow_seeds board point_of_view pit_index in
      let next_point_of_view = other_view point_of_view in
        game_loop new_board next_point_of_view

