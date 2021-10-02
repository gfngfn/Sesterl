
open MyUtil
open Syntax
open IntermediateSyntax
open Env
open Errors


module SigRecordMap = Map.Make(String)

type sig_record_map = ((signature_source * SigRecord.t) abstracted * space_name) SigRecordMap.t

type single_output = {
  module_name : module_name;
  signature   : (signature_source * SigRecord.t) abstracted;
  space_name  : space_name;
  attribute   : ModuleAttribute.t;
  bindings    : binding list;
}


let check_single ~(is_verbose : bool) ~(is_main_module : bool) (sigrmap : sig_record_map) (tyenv_before : Typeenv.t) (source : SourceLoader.loaded_module) : (signature_source * SigRecord.t) abstracted * single_output =
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

      | Some(((_, (isig, sigr)), sname)) ->
          let mentry =
            {
              mod_signature = (isig, ConcStructure(sigr));
              mod_name      = sname;
              mod_doc       = None;
            }
          in
          tyenv |> Typeenv.add_module depmodnm mentry
    ) tyenv_before
  in
  let (_, modnm) = modident in
  let absmodsigopt =
    let tyenv_for_sig = if is_main_module then tyenv_before else tyenv_for_mod in
    let address = Address.root |> Address.append_member modnm in
    utsigopt |> Option.map (Typechecker.typecheck_signature ~address tyenv_for_sig)
  in
  let (_, abssigr, sname, (modattr, ibinds)) = Typechecker.main tyenv_for_mod modident absmodsigopt utmod in
  let out =
    {
      module_name = modnm;
      signature   = abssigr;
      space_name  = sname;
      attribute   = modattr;
      bindings    = ibinds;
    }
  in
  (abssigr, out)


let main ~(is_verbose : bool) (tyenv_before : Typeenv.t) ~aux:(auxmods : SourceLoader.loaded_module list) ~main:(mainmod : SourceLoader.loaded_module) ~test:(testmods : SourceLoader.loaded_module list) : Typeenv.t * single_output list * single_output * single_output list =
  let (sigrmap, auxoutacc) =
    auxmods |> List.fold_left (fun (sigrmap, auxoutacc) auxmod ->
      let (abssigr, auxout) =
        check_single ~is_verbose ~is_main_module:false sigrmap tyenv_before auxmod
      in
      let sname = auxout.space_name in
      let sigrmap =
        let (_, modnm) = auxmod.SourceLoader.module_identifier in
        sigrmap |> SigRecordMap.add modnm (abssigr, sname)
      in
      let auxoutacc = Alist.extend auxoutacc auxout in
      (sigrmap, auxoutacc)
    ) (SigRecordMap.empty, Alist.empty)
  in

  let ((_, (mainisig, mainsigr)), mainout) =
    check_single ~is_verbose ~is_main_module:true sigrmap tyenv_before mainmod
  in

  let (_sigrmap, testoutacc) =
    testmods |> List.fold_left (fun (sigrmap, testoutacc) testmod ->
      let (abssigr, testout) =
        check_single ~is_verbose ~is_main_module:false sigrmap tyenv_before testmod
      in
      let sname = testout.space_name in
      let sigrmap =
        let (_, modnm) = testmod.SourceLoader.module_identifier in
        sigrmap |> SigRecordMap.add modnm (abssigr, sname)
      in
      let testoutacc = Alist.extend testoutacc testout in
      (sigrmap, testoutacc)
    ) (sigrmap, Alist.empty)
  in

  let tyenv =
    let (_, mainmod) = mainmod.SourceLoader.module_identifier in
    let mainsname = mainout.space_name in
    let mentry =
      {
        mod_signature = (mainisig, ConcStructure(mainsigr));
        mod_name      = mainsname;
        mod_doc       = None;
      }
    in
    tyenv_before |> Typeenv.add_module mainmod mentry
  in
  let auxouts = Alist.to_list auxoutacc in
  let testouts = Alist.to_list testoutacc in
  (tyenv, auxouts, mainout, testouts)
