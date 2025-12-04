open Lwt
open Lwt.Syntax

let read_file fname =
  let ic = open_in fname in
  let s = really_input_string ic @@ in_channel_length ic in
  close_in ic;
  s

let main () =
  let url = read_file "url" in
  let token = read_file "token" in
  Printf.printf "Testing: %s\n%!" url;
  Printf.printf "Token: %s\n%!" token;
  let h = HotCRP.make url ~token in
  let* p = HotCRP.get_paper h 650 in
  ignore p;
  print_endline "Got paper!";
  return ()

let () =
  Lwt_main.run @@ main ()
