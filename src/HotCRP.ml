open Lwt
open Cohttp
open Cohttp_lwt_unix

type t =
  {
    url : string;
    token : string;
  }

let make ~token url =
  let url = if String.ends_with ~suffix:"/" url then String.sub url 0 (String.length url - 1) else url in
  { url; token }

let get_json h path =
  let uri = Uri.of_string (h.url ^ path) in
  Printf.printf "uri : %s\n%!" (Uri.to_string uri);
  let headers = Header.add (Header.init ()) "Authorization" ("Bearer " ^ h.token) in
  Client.get ~headers uri >>= fun (_resp, body) ->
  (* body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string *)
  let open Lwt.Syntax in
  let* s = body |> Cohttp_lwt.Body.to_string in
  print_endline s;
  return ()

type paper =
  {
    pid : int;
    title : string;
    abstract : string;
    authors : string list;
    submitted : bool;
  }

let get_paper h id =
  (* let open Yojson.Safe.Util in *)
  (* let json = get_json h ("/api/paper/" ^ string_of_int id) in *)
  (* { *)
    
  (* } *)
  get_json h ("/api/paper?p=" ^ string_of_int id)
