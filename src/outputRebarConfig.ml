
open MyUtil


type value =
  | Int    of int
  | String of string
  | Atom   of string
  | Bool   of bool
  | List   of value list
  | Keyed  of string * value list
  | Assoc  of assoc

and assoc =
  (string * value) list


let rec stringify_value = function
  | Int(n)    -> string_of_int n
  | String(s) -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Atom(s)   -> s

  | Bool(true)  -> "true"
  | Bool(false) -> "false"


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


let make_git_spec (git_spec : ConfigLoader.git_spec) =
  match git_spec with
  | Tag(s)    -> keyed "tag" [ String(s) ]
  | Ref(s)    -> keyed "ref" [ String(s) ]
  | Branch(s) -> keyed "branch" [ String(s) ]


let make (config : ConfigLoader.config) : assoc =
  let entry_plugins =
    let v_git_spec = keyed "branch" [ String "master" ] in
    "plugins" ==> Assoc[
      Constants.plugin_name ==> keyed "git" [ String(Constants.plugin_url); v_git_spec ]
    ]
  in
  let reldir_out = config.erlang_config.output_directory in
  let reldir_test_out = config.erlang_config.test_output_directory in
  let entry_src_dirs =
    let reldirs = (reldir_out :: config.source_directories) in
    "src_dirs" ==> List(reldirs |> List.map relative_dir_to_string)
  in
  let entry_eunit_tests =
    let reldirs = (reldir_test_out :: config.test_directories) in
    "eunit_tests" ==> List(reldirs |> List.map (fun reldir -> Keyed("dir", [ relative_dir_to_string reldir ])))
  in
  let entry_deps =
    let deps_sesterl =
      config.dependencies |> List.fold_left (fun acc dep ->
        let name = dep.ConfigLoader.dependency_name in
        match dep.ConfigLoader.dependency_source with
        | Local(_) ->
            acc

        | Git{ repository = uri; git_spec = git_spec } ->
            let v_git_spec = make_git_spec git_spec in
            let v_dep = keyed "git" [ String(uri); v_git_spec ] in
            Alist.extend acc (name, v_dep)
      ) Alist.empty |> Alist.to_list
    in
    let deps_erlang =
      config.erlang_config.erlang_dependencies |> List.map (fun erldep ->
        let name = erldep.ConfigLoader.erlang_library_name in
        let v_dep =
          match erldep.ConfigLoader.erlang_library_source with
          | ErlangLibFromHex{ version = version } ->
              String(version)

          | ErlangLibFromGit{ repository = uri; git_spec = git_spec } ->
              let v_git_spec = make_git_spec git_spec in
              keyed "git" [ String(uri); v_git_spec ]
        in
        (name, v_dep)
      )
    in
    "deps" ==> Assoc(List.append deps_sesterl deps_erlang)
  in
  let entries_relx =
    let open ConfigLoader in
    match config.erlang_config.relx with
    | None ->
        []

    | Some(relx) ->
        let release = relx.relx_release in
        let entry =
          "relx" ==> List[
            Keyed("release", [
              Keyed(release.relx_name, [ String(release.relx_version) ]);
              List(
                release.relx_applications |> List.map (fun app -> Atom(app))
              );
            ]);
            Keyed("dev_mode", [ Bool(relx.relx_dev_mode) ])
          ]
        in
        [ entry ]
  in
  let entry_sesterl_opts =
    "sesterl_opts" ==> Assoc[
      "output_dir" ==> relative_dir_to_string reldir_out
    ]
  in
  List.concat [
    [
      entry_plugins;
      entry_src_dirs;
      entry_deps;
      entry_eunit_tests;
    ];
    entries_relx;
    [
      entry_sesterl_opts;
    ];
  ]


let main (absdir_out : absolute_dir) (config : ConfigLoader.config) =
  let top_assoc = make config in
  let s =
    top_assoc |> List.map (fun (key, v) ->
      Printf.sprintf "{%s, %s}.\n" key (stringify_value v)
    ) |> String.concat ""
  in
  let fpath_out = Filename.concat absdir_out "rebar.config" in
  let fout = open_out fpath_out in
  output_string fout s;
  close_out fout;
  Logging.output_written fpath_out
