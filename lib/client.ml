open Lwt.Infix
module Util = Util
module Bytes = Util.Bytes
module Socket = Util.Socket
open Lwt.Syntax

(* Usage of Lwt.choose :
    There are places in the code where Lwt.choose is used,
    this is to help establish a non-blocking way of making sure
    both types of handlers - send and receive work in the background
    and when they are resolved they get a chance to generate result
*)

let client_trip = ref @@ Util.init_trip ()

let create_socket host port =
  let open Lwt_unix in
  let* sockaddr = Socket.get_addr host port in
  let socket = Socket.create () in
  let create () = connect socket sockaddr >>= fun _ -> Lwt.return socket in
  handle_unix_error create ()

let rec handle_send socket =
  Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ") >>= fun _ ->
  Lwt_io.read_line_opt Lwt_io.stdin >>= fun input ->
  match input with
  | Some msg ->
      let bytes_msg = Bytes.of_string msg in
      Lwt_io.(write_line stdout @@ Printf.sprintf "Sending message : %s" msg)
      >>= fun _ ->
      (* send data as bytes to the server *)
      Lwt_unix.write socket bytes_msg 0 (Bytes.length bytes_msg) >>= fun _ ->
      (* initialize start time *)
      client_trip :=
        Util.update_trip !client_trip (Unix.gettimeofday ())
          !client_trip.end_time;
      Lwt.choose [ handle_send socket; handle_recv socket ]
  | None -> Lwt.choose [ handle_send socket; handle_recv socket ]

and handle_recv socket =
  let buffer = Bytes.create 1024 in
  (* receive data as bytes from the server *)
  Lwt_unix.read socket buffer 0 1024 >>= fun len ->
  (* initialize end time *)
  client_trip :=
    Util.update_trip !client_trip !client_trip.start_time
    @@ Unix.gettimeofday ();
  let buffer = Bytes.to_string @@ Bytes.filter_unfilled_bytes buffer '\000' in
  (* check if buffer is empty *)
  if len == 0 then handle_recv socket
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
        Lwt.choose [ handle_send socket; handle_recv socket ]
    | _ ->
        Lwt_unix.write socket Util.ack 0 Util.ack_len >>= fun _ ->
        Lwt_io.(
          write_line stdout @@ Printf.sprintf "Received message: %s" buffer)
        >>= fun _ ->
        (* re-initialize the value for the next trip *)
        client_trip := Util.init_trip ();
        Lwt.choose [ handle_send socket; handle_recv socket ]

let start_client host port =
  let* socket = create_socket host port in
  let rec loop () =
    (* wait for promise resolution *)
    Lwt.choose [ handle_recv socket; handle_send socket ] >>= fun _ -> loop ()
  in
  loop ()

let main host port = Lwt_main.run @@ start_client host port
