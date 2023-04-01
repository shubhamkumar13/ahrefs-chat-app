module Server = Server
module Client = Client

let test_chatapp () =
  Alcotest.(check string) "same string" Server.(f ()) "hello"

let suite = [ Alcotest.test_case "Test chatapp" `Quick test_chatapp ]
let () = Alcotest.run "Chatapp tests" [ ("Chatapp", suite) ]
