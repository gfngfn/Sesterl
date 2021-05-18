
open MyUtil
open Syntax
open IntermediateSyntax
open Env
open Errors


module SigRecordMap = Map.Make(String)

type sig_record_map = (SigRecord.t abstracted * space_name) SigRecordMap.t

type single_output = {
  space_name  : space_name;
  attribute   : ModuleAttribute.t;
  bindings    : binding list;
  is_for_test : bool;
}


let check_single (is_verbose : bool) ~check_public_signature:(check_public_signature : bool) (sigrmap : sig_record_map) (tyenv_before : Typeenv.t) (source : SourceLoader.loaded_module) : SigRecord.t abstracted * single_output =
  let abspath  = source.SourceLoader.source_path in
  let modident = source.SourceLoader.module_identifier in
  let utsigopt = source.SourceLoader.signature in
  let utmod    = source.SourceLoader.module_content in
  let deps     = source.SourceLoader.dependencies in
  Logging.begin_to_typecheck abspath;

  let tyenv_for_mod =
    deps |> List.fold_left (fun tyenv (rng, depmodnm) ->
      match sigrmap |> SigRecordMap.find_opt depmodnm with
      | None ->
        (* Only the main module of the given package can cause this error;
           The dependency between submodules has already been checked by `SourceLoader`
           when submodules are topologically sorted. *)
          raise (ConfigError(ModuleNotFound(rng, depmodnm)))

      | Some(((_, sigr), sname)) ->
          tyenv |> Typeenv.add_module depmodnm (ConcStructure(sigr)) sname
    ) tyenv_before
  in
  let absmodsigopt =
    let tyenv_for_sig = if check_public_signature then tyenv_before else tyenv_for_mod in
    utsigopt |> Option.map (Typechecker.typecheck_signature Alist.empty tyenv_for_sig)
  in
  let (_, abssigr, sname, (modattr, ibinds)) = Typechecker.main tyenv_for_mod modident absmodsigopt utmod in
(*
  let (_, sigr) = abssigr in
  if is_verbose then display_top_structure modident sigr;
*)
  let out =
    {
      space_name  = sname;
      attribute   = modattr;
      bindings    = ibinds;
      is_for_test = source.SourceLoader.is_in_test_dirs;
    }
  in
  (abssigr, out)


let main (is_verbose : bool) (tyenv_before : Typeenv.t) (submods : SourceLoader.loaded_module list) (mainmod : SourceLoader.loaded_module) : Typeenv.t * single_output list =
  let (sigrmap, suboutacc) =
    submods |> List.fold_left (fun (sigrmap, suboutacc) source ->
      let (abssigr, out) = check_single is_verbose ~check_public_signature:false sigrmap tyenv_before source in
      let sname = out.space_name in
      let sigrmap =
        let (_, modnm) = source.SourceLoader.module_identifier in
        sigrmap |> SigRecordMap.add modnm (abssigr, sname)
      in
      let suboutacc = Alist.extend suboutacc out in
      (sigrmap, suboutacc)
    ) (SigRecordMap.empty, Alist.empty)
  in

  let ((_, mainsigr), mainout) =
    check_single is_verbose ~check_public_signature:true sigrmap tyenv_before mainmod
  in
  let tyenv =
    let (_, mainmod) = mainmod.SourceLoader.module_identifier in
    let mainsname = mainout.space_name in
    tyenv_before |> Typeenv.add_module mainmod (ConcStructure(mainsigr)) mainsname
  in
  let outs = Alist.to_list (Alist.extend suboutacc mainout) in
  (tyenv, outs)
