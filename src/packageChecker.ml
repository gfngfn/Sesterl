
open MyUtil
open Env


let main (is_verbose : bool) (tyenv_before : Typeenv.t) (sources : SourceLoader.loaded_module list) =
  let (_, outacc) =
    sources |> List.fold_left (fun (tyenv, outacc) source ->
      let abspath = source.SourceLoader.source_path in
      let modident = source.SourceLoader.module_identifier in
      let utmod = source.SourceLoader.module_content in
      Logging.begin_to_typecheck abspath;
      let (tyenv, (oidset, sigr), sname, binds) = Typechecker.main tyenv modident utmod in
      if is_verbose then display_structure 0 sigr;
      let outacc = Alist.extend outacc (sname, binds) in
      (tyenv, outacc)
    ) (tyenv_before, Alist.empty)
  in
  Alist.to_list outacc
