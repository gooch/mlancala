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

type command =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Quit

type valid_move =
  | Some of int
  | None

type winner =
  | Some of point_of_view
  | None

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

let capturable_pit board index =
  board.pits.(index) = 0
  && ((board.player_turn = Player1 && index >= 0 && index < 6)
      || (board.player_turn = Player2 && index >= 7 && index < 13))
;;

let rec distribute_seeds board index (seeds : int) =
  let point_of_view = board.player_turn in
  let new_index = (index + 1) mod 14 in
  if seeds = 0
  then if store_index point_of_view = index then board else change_player board
  else if index_is_store new_index && store_index point_of_view <> new_index
  then distribute_seeds board new_index seeds
  else if capturable_pit board new_index && seeds = 1
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

let sow_seeds board index =
  Printf.printf "index: %d" index;
  let current_index =
    match board.player_turn with
    | Player1 -> index
    | Player2 -> index + 7
  in
  let seeds = board.pits.(current_index) in
  board.pits.(current_index) <- 0;
  distribute_seeds board current_index seeds
;;

let read_character () =
  Printf.printf "> ";
  match read_line () with
  | exception End_of_file -> failwith "err end of file"
  | line -> line
;;

let remaining_seeds board =
  let a =
    match board.player_turn with
    | Player1 -> 0
    | Player2 -> 7
  in
  Array.fold_left ( + ) 0 (Array.sub board.pits a 6)
;;

let win_condition board = remaining_seeds board == 0

let winning_player board =
  if board.pits.(store_index Player1) > board.pits.(store_index Player2)
  then Some Player1
  else if board.pits.(store_index Player2) > board.pits.(store_index Player1)
  then Some Player2
  else None
;;

let rec get_move board =
  Printf.printf
    "%s, enter your move (pit index 1-6): "
    (point_of_view_to_string board.player_turn);
  if win_condition board
  then (
    Printf.printf
      "\n%s has no remaining moves.\n"
      (point_of_view_to_string board.player_turn);
    let other_player =
      match board.player_turn with
      | Player1 -> Player2
      | Player2 -> Player1
    in
    let current_store = board.pits.(store_index other_player) in
    board.pits.(store_index other_player)
    <- current_store + remaining_seeds (change_player board);
    print_board board;
    let () =
      match winning_player board with
      | Some pov -> Printf.printf "%s wins!\n" (point_of_view_to_string pov)
      | None -> Printf.printf "Draw. You're both losers."
    in
    Quit)
  else (
    match read_character () with
    | "1" -> One
    | "2" -> Two
    | "3" -> Three
    | "4" -> Four
    | "5" -> Five
    | "6" -> Six
    | "q" -> Quit
    | _ -> get_move board)
;;

let valid_move cmd : valid_move =
  match cmd with
  | One -> Some 0
  | Two -> Some 1
  | Three -> Some 2
  | Four -> Some 3
  | Five -> Some 4
  | Six -> Some 5
  | _ -> None
;;

let doable_move board (index : valid_move) : valid_move =
  match index with
  | Some i ->
    let ii =
      match board.player_turn with
      | Player1 -> i
      | Player2 -> i + 7
    in
    if board.pits.(ii) > 0
    then Some i
    else (
      print_endline "That pit is empty";
      None)
  | None -> None
;;

let rec game_loop board =
  print_board board;
  let c = get_move board in
  match c with
  | Quit ->
    print_endline "Bye.";
    exit 0
  | _ ->
    (match doable_move board (valid_move c) with
     | Some index ->
       let new_board = sow_seeds board index in
       game_loop new_board
     | None -> game_loop board)
;;
