open Mancala
open Network
open Unix

let server_address = Unix.inet_addr_any
let server_port = 9000

let get_player_sockets p1_fd p2_fd board =
  if board.player_turn = Player1 then (p1_fd, p2_fd) else (p2_fd, p1_fd)

let safe_send client_fd message =
  match Network.send_message client_fd message with
  | Ok () -> ()
  | Error err_msg ->
    Printf.eprintf "Error sending message: %s\n" err_msg;
    (* Consider this a disconnect or serious error with this client *)
    raise (Failure ("Network send failed: " ^ err_msg))
;;

let safe_close_socket fd label =
  try Unix.close fd with
  | Unix_error (err, _, _) ->
    Printf.eprintf "Error closing socket %s: %s\n" label (Unix.error_message err)
  | exn -> Printf.eprintf "Unexpected error closing socket %s: %s\n" label (Printexc.to_string exn)
;;

let notify_opponent_and_shutdown opponent_fd reason_msg listen_socket client1_fd client2_fd_opt =
  Printf.eprintf "Notifying opponent and shutting down: %s\n" reason_msg;
  (match client2_fd_opt with
   | Some client2_fd ->
     if opponent_fd == client1_fd then safe_send client2_fd (Network.Error ("Opponent disconnected: " ^ reason_msg))
     else safe_send client1_fd (Network.Error ("Opponent disconnected: " ^ reason_msg))
   | None -> () (* Only one client was connected *)
  );
  safe_close_socket listen_socket "listen_socket";
  safe_close_socket client1_fd "client1_fd";
  (match client2_fd_opt with
   | Some client2_fd -> safe_close_socket client2_fd "client2_fd"
   | None -> ());
  exit 1
;;


let rec game_loop (p1_fd : Unix.file_descr) (p2_fd : Unix.file_descr) (current_board : Mancala.board) listen_socket =
  Printf.printf "Current turn: %s\n" (Mancala.point_of_view_to_string current_board.player_turn);
  Mancala.print_board current_board; (* Server-side board printing for debugging *)

  if Mancala.win_condition current_board then (
    Printf.printf "Game over. Determining winner...\n";
    (* If one player has no more seeds, the other player captures remaining seeds *)
    let final_board =
      let player_with_no_seeds = current_board.player_turn in
      let other_player = match player_with_no_seeds with Player1 -> Player2 | Player2 -> Player1 in
      let seeds_to_capture = Mancala.remaining_seeds { current_board with player_turn = other_player } in
      current_board.pits.(Mancala.store_index other_player) <- current_board.pits.(Mancala.store_index other_player) + seeds_to_capture;
      (* Zero out the other player's pits *)
      let start_pit_idx, end_pit_idx = match other_player with Player1 -> (0,5) | Player2 -> (7,12) in
      for i = start_pit_idx to end_pit_idx do
        current_board.pits.(i) <- 0
      done;
      current_board
    in
    let winner = Mancala.winning_player final_board in
    safe_send p1_fd (Network.BoardUpdate final_board);
    safe_send p2_fd (Network.BoardUpdate final_board);
    safe_send p1_fd (Network.GameEnd winner);
    safe_send p2_fd (Network.GameEnd winner);
    Printf.printf "Game ended. Winner: %s\n"
      (match winner with Some p -> Mancala.point_of_view_to_string p | None -> "Draw");
    () (* End of game *)
  ) else (
    let client_fd_current, client_fd_opponent = get_player_sockets p1_fd p2_fd current_board in
    try
      safe_send client_fd_current (Network.BoardUpdate current_board);
      safe_send client_fd_opponent (Network.BoardUpdate current_board);
      safe_send client_fd_current Network.YourTurn;
      safe_send client_fd_opponent (Network.OpponentTurn current_board.player_turn);

      let rec get_valid_move attempts_left =
        if attempts_left = 0 then (
          safe_send client_fd_current (Network.Error "Too many invalid attempts. Skipping turn.");
          game_loop p1_fd p2_fd (Mancala.change_player current_board) listen_socket (* Skip turn *)
        ) else (
          Printf.printf "Waiting for move from %s...\n" (Mancala.point_of_view_to_string current_board.player_turn);
          match Network.receive_message client_fd_current with
          | Ok (Network.Move pit_idx) ->
            Printf.printf "Received move: %d from %s\n" pit_idx (Mancala.point_of_view_to_string current_board.player_turn);
            if Mancala.doable_move current_board pit_idx then (
              let new_board = Mancala.sow_seeds current_board pit_idx in
              game_loop p1_fd p2_fd new_board listen_socket
            ) else (
              Printf.printf "Invalid move from %s: pit %d is not doable.\n" (Mancala.point_of_view_to_string current_board.player_turn) pit_idx;
              safe_send client_fd_current (Network.InvalidMove "Chosen pit is empty or invalid.");
              get_valid_move (attempts_left -1) (* Ask for move again *)
            )
          | Ok other_msg ->
            let msg_info = match other_msg with
                           | BoardUpdate _ -> "BoardUpdate" | PlayerAssignment _ -> "PlayerAssignment"
                           | WaitingForOpponent -> "WaitingForOpponent" | YourTurn -> "YourTurn"
                           | OpponentTurn _ -> "OpponentTurn" | GameEnd _ -> "GameEnd"
                           | Error s -> "Error: " ^ s | InvalidMove s -> "InvalidMove: " ^ s
                           | Move _ -> "Move" (* Should not happen here *)
            in
            Printf.eprintf "Received invalid message type from current player: %s\n" msg_info;
            safe_send client_fd_current (Network.Error ("Invalid message type received. Expected Move. Got: " ^ msg_info));
            get_valid_move (attempts_left - 1) (* Ask for move again *)
          | Error err_msg ->
            Printf.eprintf "Error receiving message from current player: %s. Assuming disconnect.\n" err_msg;
            notify_opponent_and_shutdown client_fd_opponent ("Current player disconnected: " ^ err_msg) listen_socket p1_fd (Some p2_fd);
            () (* Should not reach here due to exit in notify_opponent_and_shutdown *)
        )
      in
      get_valid_move 3 (* Allow 3 attempts for a valid move *)

    with
    | Failure msg when String.starts_with ~prefix:"Network send failed:" msg ->
        let disconnected_player = if client_fd_current == p1_fd then "Player 1" else "Player 2" in
        let reason = Printf.sprintf "%s seems to have disconnected during send: %s" disconnected_player msg in
        notify_opponent_and_shutdown client_fd_opponent reason listen_socket p1_fd (Some p2_fd)
    | exn ->
        let error_str = Printexc.to_string exn in
        Printf.eprintf "Unhandled exception in game loop for player %s: %s\nBacktrace:\n%s\n"
          (Mancala.point_of_view_to_string current_board.player_turn)
          error_str (Printexc.get_backtrace ());
        let reason = "Unhandled server error: " ^ error_str in
        notify_opponent_and_shutdown client_fd_opponent reason listen_socket p1_fd (Some p2_fd)
  )
;;

let start_server () =
  Printf.printf "Setting up server on port %d...\n" server_port;
  let listen_socket_res = Network.setup_server (Unix.string_of_inet_addr server_address) server_port in
  match listen_socket_res with
  | Error err_msg ->
    Printf.eprintf "Failed to setup server: %s\n" err_msg;
    exit 1
  | Ok listen_socket ->
    Printf.printf "Server listening on port %d. Waiting for connections...\n" server_port;
    let client1_fd_res = Network.accept_connection listen_socket in
    (match client1_fd_res with
     | Error err_msg ->
       Printf.eprintf "Failed to accept first client: %s\n" err_msg;
       safe_close_socket listen_socket "listen_socket";
       exit 1
     | Ok (p1_fd, p1_addr) ->
       Printf.printf "Player 1 connected from %s.\n" (match p1_addr with ADDR_INET(addr,port) -> (string_of_inet_addr addr) ^ ":" ^ (string_of_int port) | _ -> "unknown");
       safe_send p1_fd (Network.PlayerAssignment Mancala.Player1);
       safe_send p1_fd Network.WaitingForOpponent;

       let client2_fd_res = Network.accept_connection listen_socket in
       (match client2_fd_res with
        | Error err_msg ->
          Printf.eprintf "Failed to accept second client: %s\n" err_msg;
          safe_send p1_fd (Network.Error "Failed to connect second player. Server shutting down.");
          safe_close_socket p1_fd "p1_fd";
          safe_close_socket listen_socket "listen_socket";
          exit 1
        | Ok (p2_fd, p2_addr) ->
          Printf.printf "Player 2 connected from %s.\n" (match p2_addr with ADDR_INET(addr,port) -> (string_of_inet_addr addr) ^ ":" ^ (string_of_int port) | _ -> "unknown");
          safe_send p2_fd (Network.PlayerAssignment Mancala.Player2);
          Printf.printf "Both players connected. Starting game.\n";

          let initial_board = Mancala.init_board () in
          try
            game_loop p1_fd p2_fd initial_board listen_socket;
            Printf.printf "Game finished normally.\n"
          with
          | Failure msg when String.starts_with ~prefix:"Network send failed:" msg ->
             Printf.eprintf "Game terminated due to network send failure: %s\n" msg
             (* Sockets are expected to be handled by the failure point or finally block *)
          | ex ->
             Printf.eprintf "Unexpected error during game: %s\nBacktrace:\n%s\n" (Printexc.to_string ex) (Printexc.get_backtrace ());
             safe_send p1_fd (Network.Error "Critical server error. Game ending.");
             safe_send p2_fd (Network.Error "Critical server error. Game ending.")
          finally (
            Printf.printf "Closing client and server sockets.\n";
            safe_close_socket p1_fd "p1_fd";
            safe_close_socket p2_fd "p2_fd";
            safe_close_socket listen_socket "listen_socket"
          );
          Printf.printf "Server shutdown complete.\n"
       )
    )
;;

let () = start_server ()
;;
