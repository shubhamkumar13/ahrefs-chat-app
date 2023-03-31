open Lwt
open Lwt.Syntax
open Lwt.Infix
module Util = Chatlib.Util
module Bytes = Chatlib.Util.Bytes

let client_trip = ref @@ Util.init_trip ()

let port =
  try
    match int_of_string Sys.argv.(1) with
    | x when x > 1024 && x < 49152 -> x
    | _ -> failwith "Port number not in the valid range"
  with _ -> failwith "Please enter a valid port number"

let client_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let client_addr () =
  match
    Lwt_unix.(connect client_socket @@ ADDR_INET (Unix.inet_addr_any, port))
  with
  | exception _ -> Lwt_io.printf "Cannot connect to server at port : %d" port
  | _ -> Lwt.return_unit

let rec handle_send () =
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
      client_trip :=
        Util.update_trip !client_trip (Unix.gettimeofday ())
          !client_trip.end_time;
      Lwt.pick [ handle_send (); handle_recv () ]
  | None -> Lwt.pick [ handle_send (); handle_recv () ]

and handle_recv () =
  let buffer = Bytes.create 1024 in
  let* len = Lwt_unix.read client_socket buffer 0 1024 in
  client_trip :=
    Util.update_trip !client_trip !client_trip.start_time
    @@ Unix.gettimeofday ();
  let buffer = Bytes.to_string @@ Bytes.filter_unfilled_bytes buffer '\000' in
  if len == 0 then handle_recv ()
  else
    match buffer with
    | "message received" ->
        let* _ =
          Lwt_io.(
            write_line stdout
            @@ Printf.sprintf "Received message: %s\nRoundtrip : %f secs" buffer
            @@ (!client_trip.end_time -. !client_trip.start_time))
        in
        client_trip := Util.init_trip ();
        Lwt.pick [ handle_send (); handle_recv () ]
    | _ ->
        let* _ = Lwt_unix.write client_socket Util.ack 0 Util.ack_len in
        let* _ =
          Lwt_io.(
            write_line stdout @@ Printf.sprintf "Received message: %s" buffer)
        in
        client_trip := Util.init_trip ();
        Lwt.pick [ handle_send (); handle_recv () ]

let start_client () =
  let* _ = client_addr () in
  let rec loop () =
    let* _ = Lwt.choose [ handle_recv (); handle_send () ] in
    loop ()
  in
  loop ()

let _ = Lwt_main.run @@ start_client ()
