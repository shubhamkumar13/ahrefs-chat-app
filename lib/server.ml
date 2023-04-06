open Lwt.Infix
open Lwt.Syntax
module Util = Chatlib.Util
module Bytes = Chatlib.Util.Bytes

let start_server host port =
  let* server_socket, _ = Util.create_server_socket host port in
  Lwt_unix.listen server_socket 10;
  Lwt_io.printl "Waiting for socket connection :" >>= fun _ ->
  let rec loop () =
    let* client_socket, client_sockaddr = Lwt_unix.accept server_socket in
    Lwt_io.printf "Client connected from : %s\n"
      (Util.sockaddr_to_string client_sockaddr)
    >>= fun _ ->
    Lwt.choose
      [
        Util.send_handler client_socket; Util.server_recv_handler client_socket;
      ]
    >>= fun _ -> loop ()
  in
  loop ()

let main host port = Lwt_main.run @@ start_server host port
