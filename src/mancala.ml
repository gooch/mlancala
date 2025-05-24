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

let rec distribute_seeds board pit_index seeds_to_sow =
  let point_of_view = board.player_turn in
  let new_pit_index = (pit_index + 1) mod 14 in
  if seeds_to_sow = 0
  then board (* Player change logic removed, handled by server *)
  else if index_is_store new_pit_index && store_index point_of_view <> new_pit_index
  then distribute_seeds board new_pit_index seeds_to_sow
  else if capturable_pit board new_pit_index && seeds_to_sow = 1
  then (
    (* Capture opponent's seeds *)
    let new_store_total =
      board.pits.(store_index point_of_view)
      + board.pits.(opposite_pit new_pit_index)
      + 1 (* the capturing seed *)
    in
    board.pits.(store_index point_of_view) <- new_store_total;
    board.pits.(opposite_pit new_pit_index) <- 0;
    board.pits.(new_pit_index) <- 0; (* Clear the capturing pit *)
    board (* Return board, player change logic removed *)
    )
  else (
    (* Continue sowing *)
    let remaining_seeds_to_sow = seeds_to_sow - 1 in
    board.pits.(new_pit_index) <- board.pits.(new_pit_index) + 1;
    distribute_seeds board new_pit_index remaining_seeds_to_sow)
;;

let sow_seeds board move_index =
  (* move_index is 0-5 for the current player *)
  let actual_pit_index =
    match board.player_turn with
    | Player1 -> move_index
    | Player2 -> move_index + 7
  in
  let seeds_to_sow = board.pits.(actual_pit_index) in
  board.pits.(actual_pit_index) <- 0;
  let final_board = distribute_seeds board actual_pit_index seeds_to_sow in
  (* Determine if player gets another turn *)
  let last_sown_pit = (actual_pit_index + seeds_to_sow) mod 14 in
  if last_sown_pit = store_index board.player_turn
  then final_board (* Player landed in their own store, gets another turn *)
  else change_player final_board (* Otherwise, change player *)
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

let doable_move board (move_index : int) : bool =
  (* move_index is 0-5 *)
  let actual_pit_index =
    match board.player_turn with
    | Player1 -> move_index
    | Player2 -> move_index + 7
  in
  if actual_pit_index < 0 || actual_pit_index > 13 || index_is_store actual_pit_index
  then false (* Invalid index range or trying to select a store *)
  else board.pits.(actual_pit_index) > 0
;;
