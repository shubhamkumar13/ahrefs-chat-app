open Lwt
open Lwt.Syntax
open Lwt.Infix

module Bytes = struct
  include Bytes

  let filter (f : char -> bool) (b : bytes) : bytes =
    let char_list = b |> to_string |> String.to_seq |> List.of_seq in
    List.filter f char_list |> List.to_seq |> String.of_seq |> of_string

  let filter_unfilled_bytes byte_seq element =
    let is_filled_byte byte = byte <> element in
    filter is_filled_byte byte_seq
end

type trip = { mutable start_time : float; mutable end_time : float }

let init_trip () = { start_time = 0.; end_time = 0. }
let server_trip = ref @@ init_trip ()

let update_trip trip start_time end_time =
  trip.start_time <- start_time;
  trip.end_time <- end_time;
  trip

let ack = Bytes.of_string "message received"
let ack_len = Bytes.length ack
let port = 8080
let server_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let rec server_addr () =
  match
    Lwt_unix.(bind server_socket @@ Unix.(ADDR_INET (inet_addr_any, port)))
  with
  | exception _ ->
      Lwt_io.printf
        "The port %d is in use\n, please wait for the port to be free" port
      |> fun _ ->
      Lwt_unix.close server_socket >>= fun _ -> server_addr ()
  | _ -> Lwt.return_unit

let rec handle_send_client client_socket client_address =
  let* _ = Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ") in
  let* input = Lwt_io.read_line_opt Lwt_io.stdin in
  match input with
  | Some msg ->
      let* _ =
        Lwt_io.(write_line stdout @@ Printf.sprintf "Sending message : %s" msg)
      in
      let bytes_msg = Bytes.of_string msg in
      let* _ =
        Lwt_unix.write client_socket bytes_msg 0 (Bytes.length bytes_msg)
      in
      server_trip :=
        update_trip !server_trip (Unix.gettimeofday ()) !server_trip.end_time;
      Lwt.pick
        [
          handle_send_client client_socket client_address;
          handle_recv_client client_socket client_address;
        ]
  | None ->
      Lwt.pick
        [
          handle_send_client client_socket client_address;
          handle_recv_client client_socket client_address;
        ]

and handle_recv_client client_socket client_address =
  let buffer = Bytes.create 1024 in
  let* len = Lwt_unix.read client_socket buffer 0 1024 in
  server_trip :=
    update_trip !server_trip !server_trip.start_time @@ Unix.gettimeofday ();
  let buffer = Bytes.to_string @@ Bytes.filter_unfilled_bytes buffer '\000' in
  if len = 0 then
    let* _ = Lwt_unix.close client_socket in
    let* _ =
      Lwt_io.(
        write_line stdout
        @@ Printf.sprintf "Client disconnected: %s" client_address)
    in
    handle_recv_client client_socket client_address
  else
    match buffer with
    | "message received" ->
        let* _ =
          Lwt_io.(
            write_line stdout
            @@ Printf.sprintf "Received message : %s\nRoundtrip : %f secs"
                 buffer
                 (!server_trip.end_time -. !server_trip.start_time))
        in

        server_trip := init_trip ();
        Lwt.pick
          [
            handle_send_client client_socket client_address;
            handle_recv_client client_socket client_address;
          ]
    | _ ->
        let* _ = Lwt_unix.write client_socket ack 0 ack_len in
        let* _ =
          Lwt_io.(
            write_line stdout @@ Printf.sprintf "Received message : %s\n" buffer)
        in
        server_trip := init_trip ();

        Lwt.pick
          [
            handle_send_client client_socket client_address;
            handle_recv_client client_socket client_address;
          ]

let start_server () =
  let* _ = server_addr () in
  let _ = Lwt_unix.listen server_socket 10 in
  let* _ =
    Lwt_io.(
      write_line stdout @@ Printf.sprintf "Waiting for the socket connected :")
  in
  let rec loop () =
    let* client_socket, client_addr = Lwt_unix.accept server_socket in
    let client_address =
      match client_addr with
      | ADDR_INET (i, _) -> Unix.string_of_inet_addr i
      | ADDR_UNIX s -> s
    in
    let* _ =
      Lwt_io.(
        write_line stdout
        @@ Printf.sprintf "Client connected : %s" client_address)
    in
    let* _ =
      match
        Lwt.choose
          [
            handle_send_client client_socket client_address;
            handle_recv_client client_socket client_address;
          ]
      with
      | exception _ -> Lwt.return_unit
      | _ -> Lwt.return_unit
    in
    loop ()
  in
  loop ()

let _ = Lwt_main.run @@ start_server ()
