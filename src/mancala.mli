(* Define the types for the game board *)
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

val point_of_view_to_string : point_of_view -> string
val change_player : board -> board

(* Function to initialize the game board *)
val init_board : unit -> board
val player_pits : point_of_view -> board -> pits
val player_store : point_of_view -> board -> store

(* Function to print the current state of the board *)
val print_board : board -> unit
val opposite_pit : int -> int

(* Function to handle a player's move by sowing seeds *)
val sow_seeds : board -> int -> board
val distribute_seeds : board -> int -> int -> board
val game_loop : board -> unit
