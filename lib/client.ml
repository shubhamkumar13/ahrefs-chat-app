open Lwt.Infix
open Lwt.Syntax
module Util = Chatlib.Util
module Bytes = Chatlib.Util.Bytes

let start_client host port =
  let* client_socket, _ = Util.create_client_socket host port in
  let rec loop () =
    Lwt.choose
      [
        Util.send_handler client_socket; Util.client_recv_handler client_socket;
      ]
    >>= fun _ -> loop ()
  in
  loop ()

let main host port = Lwt_main.run @@ start_client host port
