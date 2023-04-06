module Util = Chatlib.Util
open Lwt.Syntax
open Lwt.Infix

let simulate_client_to_server (server_conn_socket, msg) client_socket =
  Util.write_to server_conn_socket msg >>= fun _ ->
  Util.read_from client_socket >>= fun (_, _, _) ->
  Util.(write_to client_socket ack) >>= fun _ ->
  Util.read_from server_conn_socket >>= fun (_, buffer, _) ->
  Lwt.return (buffer = Util.ack)

let simulate_server_to_client (client_socket, msg) server_conn_socket =
  Util.write_to client_socket msg >>= fun _ ->
  Util.read_from server_conn_socket >>= fun (_, _, _) ->
  Util.(write_to server_conn_socket ack) >>= fun _ ->
  Util.read_from client_socket >>= fun (_, buffer, _) ->
  Lwt.return (buffer = Util.ack)

let test_ack_client_to_server (_ : Lwt_switch.t) () =
  let* client_socket, _ = Util.create_client_socket "127.0.0.1" 8080 in
  let* server_socket, _ = Util.create_server_socket "127.0.0.1" 8080 in
  Lwt_unix.listen server_socket 10;
  let* server_conn_client, _ = Lwt_unix.accept server_socket in
  let msg = "Shubham" in
  let* assertion =
    simulate_client_to_server (client_socket, msg) server_conn_client
  in
  Lwt.return_unit >|= fun () ->
  Alcotest.(check bool)
    "Receiving Acknowledgement : Client to Server" assertion true

let test_ack_server_to_client (_ : Lwt_switch.t) () =
  let* client_socket, _ = Util.create_client_socket "127.0.0.1" 8080 in
  let* server_socket, _ = Util.create_server_socket "127.0.0.1" 8080 in
  Lwt_unix.listen server_socket 10;
  let* server_conn_client, _ = Lwt_unix.accept server_socket in
  let msg = "Shubham" in
  let* assertion =
    simulate_server_to_client (server_conn_client, msg) client_socket
  in
  Lwt.return_unit >|= fun () ->
  Alcotest.(check bool)
    "Receiving Acknowledgement : Client to Server" assertion true

let suite =
  [
    Alcotest_lwt.test_case "Receiving Acknowledgement : Client to Server" `Slow
      test_ack_client_to_server;
    Alcotest_lwt.test_case "Receiving Acknowledgement : Server to Client" `Slow
      test_ack_server_to_client;
  ]

let () = Lwt_main.run @@ Alcotest_lwt.run "Chatapp tests" [ ("Chatapp", suite) ]
