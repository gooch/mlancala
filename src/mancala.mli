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

type winner =
  | Some of point_of_view
  | None

val point_of_view_to_string : point_of_view -> string

(* Function to initialize the game board *)
val init_board : unit -> board

val change_player : board -> board
val player_pits : point_of_view -> board -> pits
val player_store : point_of_view -> board -> store

(* Function to print the current state of the board *)
val print_board : board -> unit

(* Utility functions related to board structure and state *)
val store_index : point_of_view -> int
val index_is_store : int -> bool
val opposite_pit : int -> int
val capturable_pit : board -> int -> bool

(* Core game logic functions *)
val sow_seeds : board -> int -> board (* expects move_index 0-5 for current player *)

(* This function is primarily a helper for sow_seeds.
   While it's a top-level function in the .ml, consider if it truly needs to be in the public interface.
   For now, including it as per direct translation of .ml structure. *)
val distribute_seeds : board -> int -> int -> board (* expects pit_index 0-13, and seeds count *)

val remaining_seeds : board -> int
val win_condition : board -> bool
val winning_player : board -> winner

(* Function to check if a move is valid *)
val doable_move : board -> int -> bool (* expects move_index 0-5 for current player *)
