open Lwt.Infix
open Lwt.Syntax
module Util = Chatlib.Util
module Bytes = Chatlib.Util.Bytes

(* Usage of Lwt.choose :
    There are places in the code where Lwt.choose is used,
    this is to help establish a non-blocking way of making sure
    both types of handlers - send and receive work in the background
    and when they are resolved they get a chance to generate result
*)

(* let client_trip = ref @@ Util.init_trip () *)

let start_client host port =
  let* client_socket, _ = Util.create_client_socket host port in
  let rec loop () =
    Lwt.choose
      [
        Util.send_handler client_socket; Util.client_recv_handler client_socket;
      ]
    >>= loop
  in
  loop ()

let main host port = Lwt_main.run @@ start_client host port
