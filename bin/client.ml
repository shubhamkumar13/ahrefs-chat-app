let client_socket = Unix.socket PF_INET SOCK_STREAM 0
let host = "127.0.0.1"
let port = 9999

let start_client () =
  Unix.(connect client_socket @@ ADDR_INET (inet_addr_of_string host, port))
  |> fun _ ->
  Printf.printf "Connected to server on port %d\n" port;
  flush stdout;
  let rec alive () =
    Printf.printf "Enter message: ";
    flush stdout;
    let send_msg = Bytes.of_string @@ Stdlib.read_line () in
    (* let start_time = Unix.gettimeofday () in *)
    let send_size =
      Unix.send client_socket send_msg 0 (Bytes.length send_msg) []
    in
    let recv_ack = Bytes.create 1024 in
    let _ = Unix.recv client_socket recv_ack 0 1024 [] in
    Printf.printf "Response: %s\n" @@ Bytes.to_string recv_ack;
    flush stdout;
    (* let server_response = Bytes.create 1024 in
       let recv_size = Unix.recv client_socket server_response 0 1024 [] in *)
    (* let end_time = Unix.gettimeofday () in *)
    (* let roundtrip_time = end_time -. start_time in *)
    (* let ack_size =
         Unix.send client_socket
           (Bytes.of_string "message received")
           0
           (Bytes.length @@ Bytes.of_string "message received")
           []
       in
       Printf.printf "Server response : %s\n" @@ Bytes.to_string message; *)
    (* flush stdout; *)
    (* Printf.printf "Roundtrip time : %f seconds\n" roundtrip_time; *)
    (* flush stdout; *)
    if Bytes.to_string send_msg = "exit" then () else alive ()
  in
  alive () |> fun _ -> Unix.close client_socket

let _ = start_client ()