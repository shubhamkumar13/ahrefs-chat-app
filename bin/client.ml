open Lwt
open Lwt.Syntax
open Lwt.Infix

let port = 8080
let client_socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let client_addr () =
  Lwt_unix.(connect client_socket @@ ADDR_INET (Unix.inet_addr_any, port))

let start_client () =
  let* _ = client_addr () in
  let rec loop () =
    let rec handle_send () =
      let* _ =
        Lwt_io.(write_line stdout @@ Printf.sprintf "Enter message : ")
      in
      let* input = Lwt_io.read_line_opt Lwt_io.stdin in
      match input with
      | Some msg ->
          let* _ =
            Lwt_io.(
              write_line stdout @@ Printf.sprintf "Sending message : %s" msg)
          in
          let bytes_msg = Bytes.of_string msg in
          let* _ =
            Lwt_unix.send client_socket bytes_msg 0 (Bytes.length bytes_msg) []
          in
          handle_send ()
      | None -> handle_send ()
    in
    let rec handle_recv () =
      let buffer = Bytes.create 1024 in
      let* len = Lwt_unix.recv client_socket buffer 0 1024 [] in
      if len == 0 then handle_recv ()
      else
        let* _ =
          Lwt_io.(
            write_line stdout
            @@ Printf.sprintf "message received\nReceived message: %s"
            @@ Bytes.to_string buffer)
        in
        handle_recv ()
    in
    let* _ = handle_recv () <?> handle_send () in
    loop ()
  in
  loop ()

let _ = Lwt_main.run @@ start_client ()
