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

(* roundtrip record *)
type trip = { mutable start_time : float; mutable end_time : float }

let init_trip () = { start_time = 0.; end_time = 0. }

let update_trip trip start_time end_time =
  trip.start_time <- start_time;
  trip.end_time <- end_time;
  trip

let ack = Bytes.of_string "message received"
let ack_len = Bytes.length ack

module Socket = struct
  let domain = Lwt_unix.PF_INET
  let type_ = Lwt_unix.SOCK_STREAM
  let protocol = 0
  let create () : Lwt_unix.file_descr = Lwt_unix.socket domain type_ protocol

  let get_addr host port =
    let* addr_info_list =
      Lwt_unix.getaddrinfo host port
        [ AI_FAMILY domain; AI_SOCKTYPE type_; AI_PROTOCOL protocol ]
    in
    match List.length addr_info_list with
    | 0 -> failwith "Address not found"
    | _ -> Lwt.return (List.hd addr_info_list).ai_addr

  let addr_to_string (sockaddr : Unix.sockaddr) : string =
    match sockaddr with
    | ADDR_INET (addr, port) ->
        Printf.sprintf "%s:%s"
          (Unix.string_of_inet_addr addr)
          (string_of_int port)
    | ADDR_UNIX str -> str
end
