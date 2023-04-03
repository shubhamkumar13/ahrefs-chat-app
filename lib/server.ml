open Lwt.Infix
open Lwt.Syntax
module Util = Util
module Bytes = Util.Bytes
module Socket = Util.Socket

(* Usage of Lwt.choose :
    There are places in the code where Lwt.choose is used,
    this is to help establish a non-blocking way of making sure
    both types of handlers - send and receive work in the background
    and when they are resolved they get a chance to generate result
*)

let server_trip = ref @@ Util.init_trip ()

(* only for testing please delete before submission *)
let f () = "hello"

let create_socket host port =
  let open Lwt_unix in
  let create () =
    let socket = Socket.create () in
    let* sockaddr = Socket.get_addr host port in
    setsockopt socket SO_REUSEADDR true;
    setsockopt_optint socket SO_LINGER (Some 5);
    bind socket sockaddr >>= fun _ ->
    listen socket 1;
    Lwt.return socket
  in
  handle_unix_error create ()

let rec handle_send_client client_socket client_address =
  Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ") >>= fun _ ->
  Lwt_io.read_line_opt Lwt_io.stdin >>= fun input ->
  match input with
  | Some msg ->
      let bytes_msg = Bytes.of_string msg in
      Lwt_io.(write_line stdout @@ Printf.sprintf "Sending message : %s" msg)
      >>= fun _ ->
      (* Send data to the client *)
      Lwt_unix.write client_socket bytes_msg 0 (Bytes.length bytes_msg)
      >>= fun _ ->
      (* initialize the start time for calculating roundtrip *)
      server_trip :=
        Util.update_trip !server_trip (Unix.gettimeofday ())
          !server_trip.end_time;
      Lwt.choose
        [
          handle_send_client client_socket client_address;
          handle_recv_client client_socket client_address;
        ]
  | None ->
      Lwt.choose
        [
          handle_send_client client_socket client_address;
          handle_recv_client client_socket client_address;
        ]

and handle_recv_client client_socket client_address =
  let buffer = Bytes.create 1024 in
  (*consume the data sent over by the client in buffer*)
  Lwt_unix.read client_socket buffer 0 1024 >>= fun len ->
  (* initialize end time for calculating rountrip *)
  server_trip :=
    Util.update_trip !server_trip !server_trip.start_time
    @@ Unix.gettimeofday ();
  (* removing empty chars helps in comparing strings *)
  let buffer = Bytes.to_string @@ Bytes.filter_unfilled_bytes buffer '\000' in
  if len = 0 then
    Lwt_unix.close client_socket >>= fun _ ->
    Lwt_io.(
      write_line stdout
      @@ Printf.sprintf "Client disconnected: %s" client_address)
    >>= fun _ -> handle_recv_client client_socket client_address
  else
    (* check if it is an acknowledgement or data sent from the server *)
    match buffer with
    | "message received" ->
        Lwt_io.(
          write_line stdout
          @@ Printf.sprintf "Received message : %s\nRoundtrip : %f secs" buffer
               (!server_trip.end_time -. !server_trip.start_time))
        >>= fun _ ->
        server_trip := Util.init_trip ();
        Lwt.choose
          [
            handle_send_client client_socket client_address;
            handle_recv_client client_socket client_address;
          ]
    | _ ->
        Lwt_unix.write client_socket Util.ack 0 Util.ack_len >>= fun _ ->
        Lwt_io.(
          write_line stdout @@ Printf.sprintf "Received message : %s" buffer)
        >>= fun _ ->
        server_trip := Util.init_trip ();

        Lwt.choose
          [
            handle_send_client client_socket client_address;
            handle_recv_client client_socket client_address;
          ]

let start_server host port =
  let* socket = create_socket host port in
  Lwt_io.(write_line stdout @@ Printf.sprintf "Waiting for socket connection :")
  >>= fun _ ->
  let rec loop () =
    Lwt_unix.accept socket >>= fun (client_socket, client_addr) ->
    (* sockaddr -> string *)
    let client_address = Socket.addr_to_string client_addr in
    Lwt_io.(
      write_line stdout @@ Printf.sprintf "Client connected : %s" client_address)
    >>= fun _ ->
    (match
       (* wait for promise resolution *)
       Lwt.choose
         [
           handle_send_client client_socket client_address;
           handle_recv_client client_socket client_address;
         ]
     with
    | _ -> Lwt.return_unit)
    >>= fun _ -> loop ()
  in
  loop ()

let main host port = Lwt_main.run @@ start_server host port
