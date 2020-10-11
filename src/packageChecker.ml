
open MyUtil
open Syntax
open Env


let check_single (is_verbose : bool) ~for_signature:(tyenv_for_sig : Typeenv.t) ~for_module:(tyenv_for_mod : Typeenv.t) (source : SourceLoader.loaded_module) : Typeenv.t * SigRecord.t abstracted * (space_name * binding list) =
  let abspath = source.SourceLoader.source_path in
  let modident = source.SourceLoader.module_identifier in
  let utsigopt = source.SourceLoader.signature in
  let utmod = source.SourceLoader.module_content in
  Logging.begin_to_typecheck abspath;
  let absmodsigopt = utsigopt |> Option.map (Typechecker.typecheck_signature tyenv_for_sig) in
  let (tyenv, abssigr, sname, binds) = Typechecker.main tyenv_for_mod modident absmodsigopt utmod in
  let (_, sigr) = abssigr in
  if is_verbose then display_structure 0 sigr;
  let out = (sname, binds) in
  (tyenv, abssigr, out)


let main (is_verbose : bool) (tyenv_before : Typeenv.t) (submods : SourceLoader.loaded_module list) (mainmod : SourceLoader.loaded_module) =
  let (tyenv_after_sub, suboutacc) =
    submods |> List.fold_left (fun (tyenv, suboutacc) source ->
      let (tyenv, _, out) = check_single is_verbose ~for_signature:tyenv ~for_module:tyenv source in
      let suboutacc = Alist.extend suboutacc out in
      (tyenv, suboutacc)
    ) (tyenv_before, Alist.empty)
  in

  let (_, (_, mainsigr), mainout) =
    check_single is_verbose ~for_signature:tyenv_before ~for_module:tyenv_after_sub mainmod
  in
  let tyenv =
    let (_, mainmod) = mainmod.SourceLoader.module_identifier in
    let (mainsname, _) = mainout in
    tyenv_before |> Typeenv.add_module mainmod (ConcStructure(mainsigr)) mainsname
  in
  let outs = Alist.to_list (Alist.extend suboutacc mainout) in
  (tyenv, outs)
