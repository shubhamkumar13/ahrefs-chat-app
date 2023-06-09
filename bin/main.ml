module Server = Server
module Client = Client

(**
  main.ml orchestrates client.ml and server.ml
  this is file the user will interact with
*)

let _ =
  let host = "127.0.0.1" in
  Printf.printf
    "Choose which port you want to run your application in \n\n\
    \     {Please make sure that the port selected \n\
    \     should be same for server and client, thank you} :\n\n\
    \    \t\t => ";
  let port =
    match int_of_string_opt @@ Stdlib.read_line () with
    | None ->
        Printf.printf "No port selected, choosing 8080 as the default port";
        8080
    | Some p -> p
  in
  Printf.printf
    "Choose which application you want to run :\n\n\
    \     1. For running a server choose 1\n\n\
    \     2. For running a client choose 2\n\n\
    \    \t\t => ";
  match Stdlib.read_line () |> int_of_string_opt with
  | Some 1 -> Server.main host port
  | Some 2 -> Client.main host port
  | _ -> failwith "Option doesn't exist, please try again"
