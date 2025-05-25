open Mancala
open Network
open Unix

let my_point_of_view : Mancala.point_of_view option ref = ref None

let get_server_details () =
  if Array.length Sys.argv = 3 then
    try
      (Sys.argv.(1), int_of_string Sys.argv.(2))
    with
    | Failure _ ->
      Printf.eprintf "Error: Port must be an integer. Using default 127.0.0.1:9000\n";
      ("127.0.0.1", 9000)
    | _ ->
      Printf.eprintf "Error: Invalid command line arguments. Using default 127.0.0.1:9000\n";
      ("127.0.0.1", 9000)
  else
    ("127.0.0.1", 9000) (* Defaults *)
;;

let clear_terminal () =
  print_string "\027[2J\027[H"; (* ANSI escape code to clear screen and move cursor to home *)
  flush stdout
;;

let rec read_player_move () : int =
  Printf.printf "Enter your move (1-6): ";
  flush stdout;
  try
    let line = read_line () in
    let choice = int_of_string line in
    if choice >= 1 && choice <= 6 then
      choice - 1 (* Convert to 0-indexed *)
    else (
      Printf.printf "Invalid input. Please enter a number between 1 and 6.\n";
      read_player_move ()
    )
  with
  | Failure _ (* int_of_string failed *)
  | End_of_file ->
    Printf.printf "Invalid input. Please enter a number.\n";
    read_player_move ()
;;

let safe_send sock msg =
  match Network.send_message sock msg with
  | Ok () -> ()
  | Error err ->
    Printf.eprintf "Error sending message to server: %s. Exiting.\n" err;
    Unix.close sock;
    exit 1
;;

let safe_close_socket fd label =
    try Unix.close fd with
    | Unix_error (err, _, _) ->
        Printf.eprintf "Error closing socket %s: %s\n" label (Unix.error_message err)
    | exn -> Printf.eprintf "Unexpected error closing socket %s: %s\n" label (Printexc.to_string exn)
;;


let client_main server_ip port =
  Printf.printf "Attempting to connect to server at %s:%d...\n" server_ip port;
  match Network.connect_to_server server_ip port with
  | Error err_msg ->
    Printf.eprintf "Failed to connect to server: %s\n" err_msg;
    exit 1
  | Ok sock ->
    Printf.printf "Connected to server.\n";
    let game_running = ref true in
    (try
       while !game_running do
         match Network.receive_message sock with
         | Ok msg ->
           (match msg with
            | Network.PlayerAssignment pov ->
              my_point_of_view := Some pov;
              clear_terminal ();
              Printf.printf "You are %s.\n" (Mancala.point_of_view_to_string pov);
              flush stdout
            | Network.BoardUpdate board ->
              clear_terminal ();
              (match !my_point_of_view with
               | Some pov -> Mancala.print_board { board with player_turn = pov } (* Show board from my perspective *)
               | None -> Mancala.print_board board (* Should not happen after assignment *)
              );
              flush stdout
            | Network.WaitingForOpponent ->
              Printf.printf "Waiting for opponent to connect...\n";
              flush stdout
            | Network.YourTurn ->
              (match !my_point_of_view with
               | Some pov -> Printf.printf "\nIt's your turn, %s.\n" (Mancala.point_of_view_to_string pov)
               | None -> Printf.printf "\nIt's your turn.\n" (* Should have POV by now *)
              );
              let pit_idx = read_player_move () in
              safe_send sock (Network.Move pit_idx)
            | Network.OpponentTurn pov ->
              Printf.printf "\nWaiting for %s's turn...\n" (Mancala.point_of_view_to_string pov);
              flush stdout
            | Network.InvalidMove error_msg ->
              Printf.printf "Server: %s\n" error_msg;
              flush stdout
            | Network.GameEnd winner_option ->
              clear_terminal();
              (match !my_point_of_view with
                | Some pov -> (
                    match Network.receive_message sock with (* Expecting a final board update based on instructions *)
                    | Ok (Network.BoardUpdate board) -> Mancala.print_board {board with player_turn = pov}
                    | Ok _ -> Printf.printf "Received unexpected message instead of final board.\n"
                    | Error e -> Printf.printf "Error receiving final board: %s\n" e
                )
                | None -> Printf.printf "Game ended, but client POV was not set.\n"
              );

              let outcome_msg =
                match winner_option with
                | Some p_winner ->
                  Printf.sprintf "%s wins!" (Mancala.point_of_view_to_string p_winner)
                | None -> "It's a draw!"
              in
              Printf.printf "\nGame Over: %s\n" outcome_msg;
              game_running := false; (* Exit loop *)
              flush stdout
            | Network.Error error_msg ->
              Printf.eprintf "Server error: %s\n" error_msg;
              game_running := false (* Consider exiting based on severity *)
           )
         | Error err_msg ->
           Printf.eprintf "Lost connection to server: %s. Exiting.\n" err_msg;
           game_running := false (* Exit loop *)
       done
     with
     | ex ->
       Printf.eprintf "An unexpected error occurred: %s\nBacktrace:\n%s\n"
         (Printexc.to_string ex) (Printexc.get_backtrace ());
       game_running := false (* Ensure loop terminates on other errors *)
     finally
       begin
         Printf.printf "Closing connection.\n";
         safe_close_socket sock "client_socket"
       end
    );
    Printf.printf "Client shutting down.\n"
;;

let () =
  let server_ip, port = get_server_details () in
  client_main server_ip port
;;
