let server_socket = Unix.socket PF_INET SOCK_STREAM 0
let host = "127.0.0.1"
let port = 9999

let start_server () =
  Unix.(bind server_socket @@ ADDR_INET (inet_addr_of_string host, port))
  |> fun _ ->
  Unix.(listen server_socket 1);
  Printf.printf "Waiting for a client to connect ...\n";
  flush stdout;
  let client_socket, address = Unix.accept server_socket in
  let rec alive () =
    let send_ack = Bytes.of_string "message received" in
    let send_ack () =
      Unix.send client_socket send_ack 0 (Bytes.length send_ack) [] |> ignore
    in
    Printf.printf "Enter message: ";
    flush stdout;
    let send_msg () = Bytes.of_string @@ Stdlib.read_line () in
    let recv_msg = Bytes.create 1024 in
    let bytes_recvd = Unix.recv client_socket recv_msg 0 1024 [] in
    Printf.printf "Client Message : %s\n" @@ Bytes.to_string recv_msg;
    flush stdout;
    if bytes_recvd > 0 then (
      send_ack ();
      alive ())
    else ()
    (* if bytes_recvd > 0 then (
         let server_ack = Bytes.of_string "message received" in
         let recv_ack =
           Unix.send client_socket server_ack 0 (Bytes.length server_ack) []
         in
         Printf.printf "Ack : %d\n" recv_ack;
         Printf.printf "\nReceived message: %s\n" @@ Bytes.to_string client_message;
         flush stdout;
         let server_message () =
           Printf.printf "Enter response: ";
           Bytes.of_string @@ Stdlib.read_line ()
         in
         Unix.send client_socket (server_message ()) 0
           (Bytes.length @@ server_message ())
           []
         |> ignore;
         alive ())
       else () *)
  in
  alive () |> ignore;
  Unix.close server_socket

let _ = start_server ()
