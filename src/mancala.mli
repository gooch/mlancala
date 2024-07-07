(* Define the types for the game board *)
type pit = int
type store = int

type board =
  { pits: pit array
  }

type pits = pit array

type point_of_view =
  | Player1
  | Player2

(* Function to initialize the game board *)
val init_board : unit -> board

val player_pits : point_of_view -> board -> pits

val player_store : point_of_view -> board -> store

(* Function to print the current state of the board *)
val print_board : board -> point_of_view -> unit

(* Function to handle a player's move by sowing seeds *)
val sow_seeds : board -> point_of_view -> int -> board

(* val game_loop : board -> point_of_view -> unit *)
