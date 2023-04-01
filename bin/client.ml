open Lwt
open Lwt.Syntax
open Lwt.Infix
module Util = Chatlib.Util
module Bytes = Chatlib.Util.Bytes

(* Usage of Lwt.choose :
    There are places in the code where Lwt.choose is used,
    this is to help establish a non-blocking way of making sure
    both types of handlers - send and receive work in the background
    and when they are resolved they get a chance to generate result
*)

let client_trip = ref @@ Util.init_trip ()
let client_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let client_addr port =
  match
    Lwt_unix.(connect client_socket @@ ADDR_INET (Unix.inet_addr_any, port))
  with
  | exception _ -> Lwt_io.printf "Cannot connect to server at port : %d" port
  | _ -> Lwt.return_unit

let rec handle_send () =
  Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ") >>= fun _ ->
  Lwt_io.read_line_opt Lwt_io.stdin >>= fun input ->
  match input with
  | Some msg ->
      let bytes_msg = Bytes.of_string msg in
      Lwt_io.(write_line stdout @@ Printf.sprintf "Sending message : %s" msg)
      >>= fun _ ->
      (* send data as bytes to the server *)
      Lwt_unix.write client_socket bytes_msg 0 (Bytes.length bytes_msg)
      >>= fun _ ->
      (* initialize start time *)
      client_trip :=
        Util.update_trip !client_trip (Unix.gettimeofday ())
          !client_trip.end_time;
      Lwt.choose [ handle_send (); handle_recv () ]
  | None -> Lwt.choose [ handle_send (); handle_recv () ]

and handle_recv () =
  let buffer = Bytes.create 1024 in
  (* receive data as bytes from the server *)
  Lwt_unix.read client_socket buffer 0 1024 >>= fun len ->
  (* initialize end time *)
  client_trip :=
    Util.update_trip !client_trip !client_trip.start_time
    @@ Unix.gettimeofday ();
  let buffer = Bytes.to_string @@ Bytes.filter_unfilled_bytes buffer '\000' in
  (* check if buffer is empty *)
  if len == 0 then handle_recv ()
  else
    (* check if it is an acknowledgement or data sent from the server *)
    match buffer with
    | "message received" ->
        Lwt_io.(
          write_line stdout
          @@ Printf.sprintf "Received message: %s\nRoundtrip : %f secs" buffer
          @@ (!client_trip.end_time -. !client_trip.start_time))
        >>= fun _ ->
        client_trip := Util.init_trip ();
        Lwt.choose [ handle_send (); handle_recv () ]
    | _ ->
        Lwt_unix.write client_socket Util.ack 0 Util.ack_len >>= fun _ ->
        Lwt_io.(
          write_line stdout @@ Printf.sprintf "Received message: %s" buffer)
        >>= fun _ ->
        (* re-initialize the value for the next trip *)
        client_trip := Util.init_trip ();
        Lwt.choose [ handle_send (); handle_recv () ]

let start_client port =
  client_addr port >>= fun _ ->
  let rec loop () =
    (* wait for promise resolution *)
    Lwt.choose [ handle_recv (); handle_send () ] >>= fun _ -> loop ()
  in
  loop ()

let main port = Lwt_main.run @@ start_client port
