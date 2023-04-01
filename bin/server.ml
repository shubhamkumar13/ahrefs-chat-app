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

let server_trip = ref @@ Util.init_trip ()
let server_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let rec server_addr port =
  (* check and handle error gracefully *)
  Lwt_unix.handle_unix_error
    Lwt_unix.(bind server_socket)
    Unix.(ADDR_INET (inet_addr_any, port))

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

let start_server port =
  server_addr port >>= fun _ ->
  Lwt_unix.listen server_socket 10 |> fun _ ->
  Lwt_io.(write_line stdout @@ Printf.sprintf "Waiting for socket connection :")
  >>= fun _ ->
  let rec loop () =
    Lwt_unix.accept server_socket >>= fun (client_socket, client_addr) ->
    (* sockaddr -> string *)
    let client_address = Util.sockaddr_to_string client_addr in
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

let main port = Lwt_main.run @@ start_server port
