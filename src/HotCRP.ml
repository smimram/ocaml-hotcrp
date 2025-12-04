open Lwt
open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

exception Error of string

type t =
  {
    url : string;
    token : string;
  }

let make ~token url =
  let url = if String.ends_with ~suffix:"/" url then String.sub url 0 (String.length url - 1) else url in
  { url; token }

let headers h =
  Header.add (Header.init ()) "Authorization" ("Bearer " ^ h.token)

let get h fct params =
  let uri = Uri.add_query_params' (Uri.of_string (h.url ^ "/api/" ^ fct)) params in
  Printf.printf "uri : %s\n%!" (Uri.to_string uri);
  let headers = headers h in
  let* resp, body = Client.get ~headers uri in
  let* body = Cohttp_lwt.Body.to_string body in
  if Http.Status.to_int @@ Http.Response.status resp <> 200 then raise (Error body);
  print_endline body;
  return body

let get_json h fct params =
  get h fct params >|= Yojson.Safe.from_string

let post h ?(params=[]) fct form =
  let uri = Uri.add_query_params' (Uri.of_string (h.url ^ "/api/" ^ fct)) params in
  let headers = Header.add (headers h) "Content-Type" "application/x-www-form-urlencoded" in
  let* resp, body = Client.post_form ~headers ~params:form uri in
  let* body = Cohttp_lwt.Body.to_string body in
  (* Printf.printf "Done: %s\n%!" body; *)
  if Http.Status.to_int @@ Http.Response.status resp <> 200 then raise (Error body)
  else return body

let assert_ok json =
  let open Yojson.Safe.Util in
  if not (to_bool @@ member "ok" json) then raise (Error (Yojson.Safe.to_string json))

let get_paper h pid =
  get_json h (string_of_int pid^"/paper") []

let get_reviews h pid =
  let* json = get_json h (string_of_int pid^"/review") [] in
  let open Yojson.Safe.Util in
  json
  |> member "reviews"
  |> to_list
  |> List.map (fun l -> to_assoc l |> List.filter_map (fun (k,v) -> try Some (k, to_string v) with _ -> None))
  |> return

let get_comments h pid =
  get_json h (string_of_int pid^"/comment") []

let get_comment h pid n =
  get_json h (string_of_int pid^"/comment") ["c",string_of_int n]

let get_tags h pid =
  let* json = get_json h (string_of_int pid^"/tags") [] in
  let open Yojson.Safe.Util in
  let split s =
    let n = String.index s '#' in
    let tag = String.sub s 0 n in
    let n = n + 1 in
    tag, String.sub s n (String.length s - n)
  in
  json |> member "tags" |> to_list |> List.map to_string |> List.map split |> return

let add_tags h pid tags =
  post h (string_of_int pid^"/tags") ["addtags",tags] >|= ignore

let add_tag h pid tag = add_tags h pid [tag]

let get_settings h =
  get_json h "settings" []
