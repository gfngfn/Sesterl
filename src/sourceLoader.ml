
open MyUtil


let listup_sources_in_directory (dir : absolute_dir) : absolute_path list =
  let filenames = Core.Sys.ls_dir dir in
  filenames |> List.filter_map (fun filename ->
    if Core.String.is_suffix filename ~suffix:".sest" then
      Some(Filename.concat dir filename)
    else
      None
  )
