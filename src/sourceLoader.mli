
open MyUtil
open Syntax
open Errors

exception ConfigError of config_error
exception SyntaxError of syntax_error

type loaded_package = {
  space_name   : space_name option;
  modules      : (absolute_path * (module_name ranged * untyped_module)) list;
  dependencies : ConfigLoader.dependency list;
}

val main : string -> loaded_package
(** Receiving a (possibly-relative) path [p] to a package configuration file (or a single source file),
    [main p] loads all the source files constituting the package into modules,
    and returns [pkg] where:
    {ul
      {- [pkg.space_name] is [Some(s)] where [s] is the space name corresponding to the name of the package
         (or [None] if [p] is a path to a single source file);}
      {- [pkg.modules] is the list of loaded modules
         in a topological order that reflects the dependency between modules; and}
      {- [pkg.dependencies] is the list of packages on which the loaded package depends.}
    } *)
