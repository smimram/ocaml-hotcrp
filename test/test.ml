let read_file fname =
  let ic = open_in fname in
  let s = really_input_string ic @@ in_channel_length ic in
  close_in ic;
  s

let () =
  let url = read_file "url" in
  let token = read_file "token" in
  Printf.printf "Testing: %s\n%!" url;
  Printf.printf "Token: %s\n%!" token;
  let h = HotCRP.make url ~token in
  (* let* tags = HotCRP.tags h 993 in *)
  (* let* () = HotCRP.add_tag h 993 "test" in *)
  (* let* _ = HotCRP.reviews h 993 in *)
  (* let* ids = HotCRP.search_ids h "" in *)
  (* print_endline "Got paper!"; *)
  (* Printf.printf "ids: %s\n%!" (String.concat "," @@ List.map string_of_int ids); *)
  (* Printf.printf "tags: %s\n%!" (String.concat ", " tags); *)
  (* let* _ = HotCRP.add_comment h 993 ~text:"This is a test" () in *)
  let p = HotCRP.Blocking.paper h 10 in
  Printf.printf "%d authors\n%!" (List.length p.authors);
  let e = HotCRP.Blocking.events h in
  Printf.printf "events: %s\n%!" (String.concat ", " e)
