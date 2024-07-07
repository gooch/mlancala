type pit = int
type store = int


type board =
  { pits: pit array
  }

type pits = pit array

type point_of_view =
  | Player1
  | Player2

(* Initialize the board with 4 seeds in each pit *)
let init_board ()  =
  { pits = [|4; 4; 4; 4; 4; 4; 0; 4; 4; 4; 4; 4; 4; 0|]
  }
;;

let player_pits point_of_view (board : board) : pits =
  match point_of_view with
  | Player1 -> Array.sub board.pits 0 6
  | Player2 -> Array.sub board.pits 7 6

let player_store point_of_view board =
  match point_of_view with
  | Player1 -> board.pits.(6)
  | Player2 -> board.pits.(13)

let print_board board point_of_view =
  Printf.printf "<><><><><><><><><><><><><><><><><><><><><><><><><><><>\n";
  let print_array arr =
    Array.iter (fun x -> Printf.printf "%d " x) arr;
    print_endline ""
  in
  print_array (player_pits point_of_view board)

let sow_seeds board point_of_view (index : int)  =
    let current_index : int = match point_of_view with
    | Player1 -> index
    | Player2 -> index + 7
  in

  let seeds = board.pits.(current_index) in
  board.pits.(current_index) <- 0;

  let rec distribute_seeds board point_of_view index (seeds : int ) =
    print_board board point_of_view;
  if seeds = 0 then
    board
  else
  let new_pit_value = (board.pits.(index) + 1) in
    let next_index = (index + 1) mod 14 in
      let new_seeds = (seeds - 1) in
      if (point_of_view = Player1 && index = 13) || (point_of_view = Player2 && index = 6) then
        distribute_seeds board point_of_view next_index seeds
      else begin
        board.pits.(index) <- new_pit_value;
        distribute_seeds board point_of_view next_index new_seeds
      end
  in
  let next_index = (current_index + 1) mod 14  in
    distribute_seeds board point_of_view next_index seeds
;;
