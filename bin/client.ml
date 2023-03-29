open Lwt
open Lwt.Syntax
open Lwt.Infix

let ack = Bytes.of_string "message received"
let ack_len = Bytes.length ack
let port = 8080
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
      Lwt.pick [ handle_send (); handle_recv () ]
  | None -> Lwt.pick [ handle_send (); handle_recv () ]

and handle_recv () =
  let buffer = Bytes.create 1024 in
  let* len = Lwt_unix.recv client_socket buffer 0 1024 [] in
  if len == 0 then handle_recv ()
  else
    let* _ =
      Lwt_io.(
        write_line stdout
        @@ Printf.sprintf "Received message: %s"
        @@ Bytes.to_string buffer)
    in
    Lwt.pick [ handle_send (); handle_recv () ]

let start_client () =
  let* _ = client_addr () in
  let rec loop () =
    let* _ = Lwt.choose [ handle_recv (); handle_send () ] in
    loop ()
  in
  loop ()

let _ = Lwt_main.run @@ start_client ()
