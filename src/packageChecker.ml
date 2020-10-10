
open MyUtil
open Syntax
open Env


let check_single (is_verbose : bool) (tyenv : Typeenv.t) (source : SourceLoader.loaded_module) : Typeenv.t * (space_name * binding list) =
  let abspath = source.SourceLoader.source_path in
  let modident = source.SourceLoader.module_identifier in
  let utsigopt = source.SourceLoader.signature in
  let utmod = source.SourceLoader.module_content in
  Logging.begin_to_typecheck abspath;
  let (tyenv, (oidset, sigr), sname, binds) = Typechecker.main tyenv modident utsigopt utmod in
  if is_verbose then display_structure 0 sigr;
  let out = (sname, binds) in
  (tyenv, out)


let main (is_verbose : bool) (tyenv_before : Typeenv.t) (submods : SourceLoader.loaded_module list) (mainmod : SourceLoader.loaded_module) =
  let (tyenv_after_sub, suboutacc) =
    submods |> List.fold_left (fun (tyenv, suboutacc) source ->
      let (tyenv, out) = check_single is_verbose tyenv source in
      let suboutacc = Alist.extend suboutacc out in
      (tyenv, suboutacc)
    ) (tyenv_before, Alist.empty)
  in

  let (_, mainout) = check_single is_verbose tyenv_after_sub mainmod in
  let outacc = Alist.extend suboutacc mainout in

  (* TODO: perform signature matching for the outer world of the package *)

  Alist.to_list outacc
