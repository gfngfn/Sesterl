
open MyUtil
open Syntax
open Errors

exception ConfigError of config_error
exception SyntaxError of syntax_error

val main : string -> space_name option * (absolute_path * (module_name ranged * untyped_module)) list
(** Receives a (possibly-relative) path to a package configuration file,
    loads all the source files constituting the package,
    and returns the modules in a topological order that reflects the dependency between modules.
 *)
