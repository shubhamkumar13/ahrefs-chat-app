module Bytes = struct
  include Bytes

  let filter (f : char -> bool) (b : bytes) : bytes =
    let char_list = b |> to_string |> String.to_seq |> List.of_seq in
    List.filter f char_list |> List.to_seq |> String.of_seq |> of_string

  let filter_unfilled_bytes byte_seq element =
    let is_filled_byte byte = byte <> element in
    filter is_filled_byte byte_seq
end

type trip = { mutable start_time : float; mutable end_time : float }

let init_trip () = { start_time = 0.; end_time = 0. }

let update_trip trip start_time end_time =
  trip.start_time <- start_time;
  trip.end_time <- end_time;
  trip

let ack = Bytes.of_string "message received"
let ack_len = Bytes.length ack