open Mancala

type message =
  | Move of int (* Pit index chosen by a player *)
  | BoardUpdate of Mancala.board (* Full game state *)
  | PlayerAssignment of Mancala.point_of_view (* Player1 or Player2 *)
  | WaitingForOpponent (* Server is waiting for the other player *)
  | YourTurn (* Inform client it's their turn *)
  | OpponentTurn of Mancala.point_of_view (* Inform client about whose turn it is *)
  | GameEnd of Mancala.winner (* Game conclusion and winner *)
  | Error of string (* For sending error messages *)
  | InvalidMove of string (* If a client attempts an invalid move *)

let message_to_bytes (msg : message) : bytes =
  Marshal.to_bytes msg [ Marshal.Closures ]
;;

let bytes_to_message (b : bytes) : (message, string) result =
  try Ok (Marshal.from_bytes b 0) with
  | Failure s -> Error (Printf.sprintf "Deserialization failed: %s" s)
  | exn -> Error (Printf.sprintf "An unexpected error occurred during deserialization: %s" (Printexc.to_string exn))
;;

(* Helper function to send all bytes *)
let rec send_all sock buf pos len =
  if len > 0
  then (
    let written = Unix.send sock buf pos len [] in
    send_all sock buf (pos + written) (len - written))
;;

(* Helper function to receive all bytes *)
let rec recv_all sock buf pos len =
  if len > 0
  then (
    let read_bytes = Unix.recv sock buf pos len [] in
    if read_bytes = 0 then raise End_of_file (* Connection closed *);
    recv_all sock buf (pos + read_bytes) (len - read_bytes))
;;

let send_message (sock : Unix.file_descr) (msg : message) : (unit, string) result =
  try
    let byte_data = message_to_bytes msg in
    let len = Bytes.length byte_data in
    let size_bytes = Bytes.create 8 in
    Bytes.set_int64_ne size_bytes 0 (Int64.of_int len);
    send_all sock size_bytes 0 8;
    send_all sock byte_data 0 len;
    Ok ()
  with
  | Unix.Unix_error (err, func, _arg) ->
    Error (Printf.sprintf "Unix error in %s: %s(%s)" func (Unix.error_message err) _arg)
  | End_of_file -> Error "Connection closed by peer during send"
  | exn -> Error (Printf.sprintf "Failed to send message: %s" (Printexc.to_string exn))
;;

let receive_message (sock : Unix.file_descr) : (message, string) result =
  try
    let size_bytes = Bytes.create 8 in
    recv_all sock size_bytes 0 8;
    let len = Int64.to_int (Bytes.get_int64_ne size_bytes 0) in
    if len < 0 || len > (1024 * 1024 * 10) (* Max 10MB message *)
    then Error (Printf.sprintf "Invalid message length received: %d" len)
    else (
      let msg_bytes = Bytes.create len in
      recv_all sock msg_bytes 0 len;
      bytes_to_message msg_bytes)
  with
  | Unix.Unix_error (err, func, _arg) ->
    Error (Printf.sprintf "Unix error in %s: %s(%s)" func (Unix.error_message err) _arg)
  | End_of_file -> Error "Connection closed by peer during receive"
  | Failure s -> Error (Printf.sprintf "Receiving message failed: %s" s)
  | exn -> Error (Printf.sprintf "Failed to receive message: %s" (Printexc.to_string exn))
;;

let setup_server (address_str : string) (port : int) : (Unix.file_descr, string) result =
  try
    let listen_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let server_addr = Unix.inet_addr_of_string address_str in
    Unix.bind listen_socket (Unix.ADDR_INET (server_addr, port));
    Unix.listen listen_socket 10; (* Backlog of 10 connections *)
    Ok listen_socket
  with
  | Unix.Unix_error (err, func, _arg) ->
    Error (Printf.sprintf "Unix error in %s: %s(%s)" func (Unix.error_message err) _arg)
  | exn -> Error (Printf.sprintf "Failed to setup server: %s" (Printexc.to_string exn))
;;

let accept_connection (listen_socket : Unix.file_descr) : (Unix.file_descr * Unix.sockaddr, string) result =
  try
    let client_socket, client_addr = Unix.accept listen_socket in
    Ok (client_socket, client_addr)
  with
  | Unix.Unix_error (err, func, _arg) ->
    Error (Printf.sprintf "Unix error in %s: %s(%s)" func (Unix.error_message err) _arg)
  | exn -> Error (Printf.sprintf "Failed to accept connection: %s" (Printexc.to_string exn))
;;

let connect_to_server (server_addr_str : string) (port : int) : (Unix.file_descr, string) result =
  try
    let client_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let server_addr = Unix.inet_addr_of_string server_addr_str in
    Unix.connect client_socket (Unix.ADDR_INET (server_addr, port));
    Ok client_socket
  with
  | Unix.Unix_error (err, func, _param) ->
    Error (Printf.sprintf "Unix error connecting to server: %s (%s)" (Unix.error_message err) func)
  | exn -> Error (Printf.sprintf "Failed to connect to server: %s" (Printexc.to_string exn))
;;
