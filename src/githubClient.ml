open Lwt
open Cohttp
open Cohttp_lwt_unix

let body (s_uri : string) =
  Client.get (Uri.of_string s_uri) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let fetch_branch (user : string) (repo : string) (branch : string) =
  let s_uri =
    Printf.sprintf "https://api.github.com/repos/%s/%s/branches/%s"
      user
      repo
      branch
  in
  let body = Lwt_main.run (body s_uri) in
  print_endline ("Received body\n" ^ body)
