
open MyUtil
open Syntax
open IntermediateSyntax
open Env
open Errors


module SigRecordMap = Map.Make(String)

type sig_record_map = (SigRecord.t abstracted * space_name) SigRecordMap.t

type single_output = {
  module_name : module_name;
  signature   : SigRecord.t abstracted;
  space_name  : space_name;
  attribute   : ModuleAttribute.t;
  bindings    : binding list;
  is_for_test : bool;
}


let check_single (is_verbose : bool) ~(is_main_module : bool) (sigrmap : sig_record_map) (tyenv_before : Typeenv.t) (source : SourceLoader.loaded_module) : SigRecord.t abstracted * single_output =
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
          let mentry =
            {
              mod_signature = ConcStructure(sigr);
              mod_name      = sname;
              mod_doc       = None;
            }
          in
          tyenv |> Typeenv.add_module depmodnm mentry
    ) tyenv_before
  in
  let absmodsigopt =
    let tyenv_for_sig = if is_main_module then tyenv_before else tyenv_for_mod in
    utsigopt |> Option.map (Typechecker.typecheck_signature ~address:Alist.empty tyenv_for_sig)
  in
  let (_, abssigr, sname, (modattr, ibinds)) = Typechecker.main tyenv_for_mod modident absmodsigopt utmod in
  let out =
    let (_, modnm) = modident in
    {
      module_name = modnm;
      signature   = abssigr;
      space_name  = sname;
      attribute   = modattr;
      bindings    = ibinds;
      is_for_test = source.SourceLoader.is_in_test_dirs;
    }
  in
  (abssigr, out)


let main (is_verbose : bool) (tyenv_before : Typeenv.t) (submods : SourceLoader.loaded_module list) (mainmod : SourceLoader.loaded_module) : Typeenv.t * single_output list * single_output =
  let (sigrmap, suboutacc) =
    submods |> List.fold_left (fun (sigrmap, suboutacc) source ->
      let (abssigr, out) = check_single is_verbose ~is_main_module:false sigrmap tyenv_before source in
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
    check_single is_verbose ~is_main_module:true sigrmap tyenv_before mainmod
  in
  let tyenv =
    let (_, mainmod) = mainmod.SourceLoader.module_identifier in
    let mainsname = mainout.space_name in
    let mentry =
      {
        mod_signature = ConcStructure(mainsigr);
        mod_name      = mainsname;
        mod_doc       = None;
      }
    in
    tyenv_before |> Typeenv.add_module mainmod mentry
  in
  let subouts = Alist.to_list suboutacc in
  (tyenv, subouts, mainout)
