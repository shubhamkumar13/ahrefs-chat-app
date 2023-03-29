open Lwt
open Lwt.Syntax
open Lwt.Infix

let port = 8080
let server_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let rec server_addr () =
  match
    Lwt_unix.(bind server_socket @@ Unix.(ADDR_INET (inet_addr_any, port)))
  with
  | exception _ -> server_addr ()
  | _ -> Lwt.return_unit

let rec handle_recv_client client_socket client_address =
  let buffer = Bytes.create 1024 in
  let* len = Lwt_unix.(recv client_socket buffer 0 1024 []) in
  if len = 0 then
    let* _ = Lwt_unix.close client_socket in
    let* _ =
      Lwt_io.(
        write_line stdout
        @@ Printf.sprintf "Client disconnected: %s" client_address)
    in
    handle_recv_client client_socket client_address
  else
    let* _ =
      Lwt_io.(
        write_line stdout
        @@ Printf.sprintf "Received message : %s"
        @@ Bytes.to_string buffer)
    in
    handle_send_client client_socket client_address

and handle_send_client client_socket client_address =
  let* _ = Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ") in
  let* input = Lwt_io.read_line_opt Lwt_io.stdin in
  match input with
  | Some msg ->
      let* _ =
        Lwt_io.(write_line stdout @@ Printf.sprintf "Sending message : %s" msg)
      in
      let bytes_msg = Bytes.of_string msg in
      let* _ =
        Lwt_unix.send client_socket bytes_msg 0 (Bytes.length bytes_msg) []
      in
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
