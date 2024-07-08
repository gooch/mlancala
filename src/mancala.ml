type pit = int
type store = int
type pits = pit array

type point_of_view =
  | Player1
  | Player2

type board =
  { pits : pit array
  ; player_turn : point_of_view
  }

let point_of_view_to_string = function
  | Player1 -> "Player 1"
  | Player2 -> "Player 2"
;;

(* Initialize the board with 4 seeds in each pit *)
let init_board () =
  { pits = [| 4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4; 0 |]; player_turn = Player1 }
;;

let change_player board =
  let other_player =
    match board.player_turn with
    | Player1 -> Player2
    | Player2 -> Player1
  in
  { pits = board.pits; player_turn = other_player }
;;

let player_pits point_of_view (board : board) : pits =
  match point_of_view with
  | Player1 -> Array.sub board.pits 0 6
  | Player2 -> Array.sub board.pits 7 6
;;

let player_store point_of_view board =
  match point_of_view with
  | Player1 -> board.pits.(6)
  | Player2 -> board.pits.(13)
;;

let print_board board =
  let point_of_view = board.player_turn in
  let opponent =
    match point_of_view with
    | Player1 -> Player2
    | Player2 -> Player1
  in
  Printf.printf "<><><><><><><><><><><><><><><><><><><><><><><><><><><>\n";
  let print_array arr =
    Array.iter (fun x -> Printf.printf "%d | " x) arr;
    print_endline ""
  in
  Printf.printf "%s: | " (point_of_view_to_string opponent);
  print_array (player_pits opponent board |> Array.to_list |> List.rev |> Array.of_list);
  Printf.printf
    "          %d --------------------- %d\n"
    (player_store opponent board)
    (player_store point_of_view board);
  Printf.printf "%s: | " (point_of_view_to_string point_of_view);
  print_array (player_pits point_of_view board)
;;

let store_index point_of_view =
  match point_of_view with
  | Player1 -> 6
  | Player2 -> 13
;;

let index_is_store index = index = 6 || index = 13
let opposite_pit index = 12 - index

let rec distribute_seeds board index (seeds : int) =
  let point_of_view = board.player_turn in
  let new_index = (index + 1) mod 14 in
  if seeds = 0
  then if store_index point_of_view = index then board else change_player board
  else if index_is_store new_index && store_index point_of_view <> new_index
  then distribute_seeds board new_index seeds
  else if board.pits.(new_index) = 0 && (not (index_is_store new_index)) && seeds = 1
  then (
    let new_store =
      board.pits.(store_index point_of_view) + board.pits.(opposite_pit new_index) + 1
    in
    board.pits.(store_index point_of_view) <- new_store;
    board.pits.(opposite_pit new_index) <- 0;
    board)
  else (
    let new_seeds = seeds - 1 in
    let pit_seeds = board.pits.(new_index) + 1 in
    board.pits.(new_index) <- pit_seeds;
    distribute_seeds board new_index new_seeds)
;;

let sow_seeds board (index : int) =
  let current_index : int =
    match board.player_turn with
    | Player1 -> index
    | Player2 -> index + 7
  in
  let seeds = board.pits.(current_index) in
  board.pits.(current_index) <- 0;
  distribute_seeds board current_index seeds
;;

let rec read_int prompt =
  Printf.printf "%s" prompt;
  match read_line () with
  | exception End_of_file -> 0
  | line ->
    (try int_of_string line with
     | Failure _ -> read_int prompt)
;;

let rec get_move board =
  Printf.printf
    "%s, enter your move (pit index 0-5): "
    (point_of_view_to_string board.player_turn);
  let selection = read_int "" in
  if selection < 0
  then 0
  else if selection < 6
  then selection
  else (
    let () = Printf.printf "bad input\n" in
    get_move board)
;;

let rec game_loop board =
  print_board board;
  let pit_index = get_move board in
  let new_board = sow_seeds board pit_index in
  game_loop new_board
;;
