
open MyUtil


type value =
  | Int    of int
  | String of string
  | List   of value list
  | Keyed  of string * value list
  | Assoc  of assoc

and assoc =
  (string * value) list


let rec stringify_value = function
  | Int(n)    -> string_of_int n
  | String(s) -> Printf.sprintf "\"%s\"" (String.escaped s)

  | List(vs) ->
      let s = vs |> List.map stringify_value |> String.concat ", " in
      Printf.sprintf "[%s]" s

  | Keyed(key, vs) ->
      let ss = vs |> List.map stringify_value in
      Printf.sprintf "{%s}" (String.concat ", " (key :: ss))

  | Assoc(assoc) ->
      let ss =
        assoc |> List.map (fun (key, v) ->
          Printf.sprintf "{%s, %s}" key (stringify_value v)
        )
      in
      Printf.sprintf "[%s]" (String.concat "," ss)


let ( ==> ) (key : string) (v : value) =
  (key, v)


let keyed (key : string) (vs : value list) =
  Keyed(key, vs)


let relative_dir_to_string (RelativeDir(s) : relative_dir) : value =
  String(s)


let make (config : ConfigLoader.config) : assoc =
  let entry_plugins =
    let plugin_name = "rebar_sesterl" in
    let url = "https://github.com/gfngfn/rebar_sesterl_plugin.git" in
    let git_spec = keyed "branch" [ String "master" ] in
    "plugins" ==> Assoc[
      plugin_name ==> keyed "git" [ String(url); git_spec ]
    ]
  in
  let reldir_out = config.erlang_config.output_directory in
  let entry_src_dirs =
    let reldirs = (reldir_out :: config.source_directories) in
    "src_dirs" ==> List(reldirs |> List.map relative_dir_to_string)
  in
  let entry_sesterl_opts =
    "sesterl_opts" ==> Assoc[
      "output_dir" ==> relative_dir_to_string reldir_out
    ]
  in
  [
    entry_plugins;
    entry_src_dirs;
    entry_sesterl_opts;
  ]


let main (absdir_out : absolute_dir) (config : ConfigLoader.config) =
  let top_assoc = make config in
  let s =
    top_assoc |> List.map (fun (key, v) ->
      Printf.sprintf "{%s, %s}." key (stringify_value v)
    ) |> String.concat "\n"
  in
  let fpath_out = Filename.concat absdir_out "rebar.config" in
  let fout = open_out fpath_out in
  output_string fout s;
  close_out fout;
  Logging.output_written fpath_out
