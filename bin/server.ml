open Lwt
open Lwt.Syntax
open Lwt.Infix

let port = 8080
let server_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let server_addr () =
  Lwt_unix.(bind server_socket @@ Unix.(ADDR_INET (inet_addr_any, port)))

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
    let rec handle_recv_client () =
      let buffer = Bytes.create 1024 in
      let* len = Lwt_unix.recv client_socket buffer 0 1024 [] in
      if len = 0 then
        let* _ = Lwt_unix.close client_socket in
        let* _ =
          Lwt_io.(
            write_line stdout
            @@ Printf.sprintf "Client disconnected: %s" client_address)
        in
        Lwt.return_unit
      else
        let* _ =
          Lwt_io.(
            write_line stdout
            @@ Printf.sprintf "message received\nReceived message : %s"
            @@ Bytes.to_string buffer)
        in
        handle_recv_client ()
    in
    let rec handle_send_client () =
      let* _ =
        Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ")
      in
      let* input = Lwt_io.read_line_opt Lwt_io.stdin in
      match input with
      | Some msg ->
          let bytes_msg = Bytes.of_string msg in
          let* _ =
            Lwt_unix.send client_socket bytes_msg 0 (Bytes.length bytes_msg) []
          in
          handle_send_client ()
      | None -> handle_send_client ()
    in
    let* _ = handle_recv_client () <?> handle_send_client () in
    loop ()
  in
  loop ()

let _ = Lwt_main.run @@ start_server ()