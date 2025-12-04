open Lwt
open Cohttp
open Cohttp_lwt_unix

type t =
  {
    url : string;
    token : string;
  }

let make ~token url =
  { url; token }

let get_json h path =
  let uri = Uri.with_path (Uri.of_string h.url) path in
  let headers = Header.add (Header.init ()) "Authorization" ("Bearer " ^ h.token) in
  Client.get ~headers uri >>= fun (_resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string

let get_paper h id =
  get_json h ("/api/paper/" ^ string_of_int id)

