module Server = Server
module Client = Client

(**
  main.ml orchestrates client.ml and server.ml
  this is file the user will interact with
*)

let _ =
  Printf.printf
    "Choose which port you want to run your application in \n\n\
    \     {Please make sure that the port selected \n\
    \     should be same for server and client, thank you} :\n\n\
    \    \t\t => ";
  let port =
    try
      match int_of_string_opt @@ Stdlib.read_line () with
      | Some p when p >= 1024 && p <= 49162 -> p
      | None ->
          Printf.printf "No port selected, choosing 8080 as the default port";
          8080
      | Some _ -> failwith "Wrong port selected"
    with _ ->
      failwith
        "Wrong range of port selected, please try again within the range of \
         [1024 - 49162]"
  in
  Printf.printf
    "Choose which application you want to run :\n\n\
    \     1. For running a server choose 1\n\n\
    \     2. For running a client choose 2\n\n\
    \    \t\t => ";
  match Stdlib.read_line () |> int_of_string_opt with
  | Some 1 -> Server.main @@ port
  | Some 2 -> Client.main @@ port
  | _ -> failwith "Option doesn't exist, please try again"
