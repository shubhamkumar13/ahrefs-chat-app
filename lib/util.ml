open Lwt.Infix
open Lwt.Syntax

module Bytes = struct
  include Bytes

  let filter (f : char -> bool) (b : bytes) : bytes =
    let char_list = b |> to_string |> String.to_seq |> List.of_seq in
    List.filter f char_list |> List.to_seq |> String.of_seq |> of_string

  let filter_unfilled_bytes byte_seq element =
    let is_filled_byte byte = byte <> element in
    filter is_filled_byte byte_seq
end

let rtt = ref 0.
let set_rtt () = rtt := Unix.gettimeofday ()
let calc_rtt () = rtt := Unix.gettimeofday () -. !rtt
let reset_rtt () = rtt := 0.
let ack = "message received"
let ack_bytes = Bytes.of_string ack
let ack_len = Bytes.length ack_bytes

let sockaddr_to_string (sockaddr : Unix.sockaddr) : string =
  match sockaddr with
  | ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port
  | ADDR_UNIX s -> s

let create_socket () = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let close_client socket =
  Lwt_unix.close socket >>= fun _ -> Lwt_io.printf "Client disconnected"

let get_sockaddr host port = Unix.(ADDR_INET (inet_addr_of_string host, port))

let create_client_socket host port =
  let socket = create_socket () in
  let sockaddr = get_sockaddr host port in
  (match Lwt_unix.connect socket sockaddr with
  | exception _ ->
      Lwt_io.printf "Cannot connect to server at : [%s:%s]" host
        (sockaddr_to_string sockaddr)
  | _ -> Lwt.return_unit)
  >>= fun _ -> Lwt.return (socket, sockaddr)

let create_server_socket host port =
  let socket = create_socket () in
  let sockaddr = get_sockaddr host port in
  (match Lwt_unix.bind socket sockaddr with
  | exception e ->
      Lwt_io.printlf "Cannot bind to client at : [%s:%s]" host
        (sockaddr_to_string sockaddr)
      >>= fun _ -> Lwt.fail e
  | _ -> Lwt.return_unit)
  >>= fun _ -> Lwt.return (socket, sockaddr)

let read_from socket =
  let buffer = Bytes.create 1024 in
  let* len = Lwt_unix.read socket buffer 0 1024 in
  calc_rtt ();
  let empty_byte = '\000' in
  let buffer =
    Bytes.(
      filter_unfilled_bytes buffer empty_byte
      |> to_string |> String.lowercase_ascii)
  in
  Lwt.return (len, buffer, !rtt)

let write_to socket msg =
  let bytes_msg = Bytes.of_string msg in
  let bytes_len = Bytes.length bytes_msg in
  Lwt_unix.write socket bytes_msg 0 bytes_len >>= fun _ ->
  set_rtt () |> Lwt.return

let rec send_handler socket =
  Lwt_io.printl "Enter message : " >>= fun _ ->
  Lwt_io.(read_line_opt stdin) >>= fun input ->
  match input with
  | Some msg ->
      Lwt_io.printf "Sending message : %s\n" msg >>= fun _ ->
      write_to socket msg >>= fun _ -> send_handler socket
  | None -> Lwt.return_unit

let rec client_recv_handler socket =
  let* len, buffer, rtt = read_from socket in
  match len with
  | 0 ->
      Lwt_unix.close socket >>= fun _ ->
      Lwt_io.printl "Server disconnected\n" >>= fun _ -> Lwt.return_unit
  | _ ->
      Lwt.catch
        (fun () ->
          if buffer = ack then (
            Lwt_io.printf "Received message : %s\nRountrip Time : %f\n" ack rtt
            >>= fun _ ->
            reset_rtt ();
            Lwt.pick [ client_recv_handler socket; send_handler socket ])
          else
            Lwt_unix.write socket ack_bytes 0 ack_len >>= fun _ ->
            Lwt_io.printf "Received message : %s\n" buffer >>= fun _ ->
            Lwt.pick [ client_recv_handler socket; send_handler socket ])
        (fun _exn ->
          Lwt_unix.close socket >>= fun _ ->
          Lwt_io.printl "Server disconnected" >>= fun _ -> Lwt.return_unit)

let rec server_recv_handler socket =
  let* len, buffer, rtt = read_from socket in
  match len with
  | 0 ->
      Lwt_unix.close socket >>= fun _ ->
      Lwt_io.printl "Client disconnected\n" >>= fun _ -> Lwt.return_unit
  | _ ->
      Lwt.catch
        (fun () ->
          if buffer = ack then (
            Lwt_io.printf "Received message : %s\nRountrip Time : %f\n" ack rtt
            >>= fun _ ->
            reset_rtt ();
            Lwt.pick [ server_recv_handler socket; send_handler socket ])
          else
            Lwt_unix.write socket ack_bytes 0 ack_len >>= fun _ ->
            Lwt_io.printf "Received message : %s\n" buffer >>= fun _ ->
            Lwt.pick [ server_recv_handler socket; send_handler socket ])
        (fun _exn ->
          Lwt_unix.close socket >>= fun _ ->
          Lwt_io.printl "Client disconnected" >>= fun _ -> Lwt.return_unit)
