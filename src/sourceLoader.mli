
open MyUtil
open Syntax
open Errors

exception SyntaxError of syntax_error

type loaded_module = {
  is_in_test_dirs   : bool;
  source_path       : absolute_path;
  module_identifier : module_name ranged;
  signature         : untyped_signature option;
  module_content    : untyped_module;
  dependencies      : (module_name ranged) list;
}

type loaded_package = {
  space_name   : space_name;
  submodules   : loaded_module list;
  main_module  : loaded_module;
}

val single : absolute_path -> loaded_module
(** Receiving an absolute path [p] to a single source file,
    [single p] loads the source file.
    May raise [ConfigError(_)] or [SyntaxError(_)].
*)

val main : requires_tests:bool -> ConfigLoader.config -> loaded_package
(** Receiving a package configuration value [config],
    [main config] loads all the source files constituting the package into modules,
    and returns [pkg] where:
    {ul
      {- [pkg.space_name] is the space name corresponding to the name of the package;}
      {- [pkg.submodules] is the list of loaded submodules
         sorted by a topological order that reflects the dependency between modules; and}
      {- [pkg.main_module] is the main module of the package.}}
    May raise [ConfigError(_)] or [SyntaxError(_)]. *)
