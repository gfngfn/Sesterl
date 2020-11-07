
open MyUtil
open Errors

exception PackageError of package_error

val load_config : absolute_dir -> ConfigLoader.config
(** [load_config absdir] loads the configuration file placed in [absdir].
    May raise [ConfigError(_)] for invalid data.
    Note that paths contained in return values of this function have not been guaranteed existent. *)

val main : absolute_dir -> (absolute_dir * ConfigLoader.config) list
(** [main absdir] lists up the package placed in [absdir] and all the packages
    on which the package depends either directly or indirectly,
    and sorts them in a topological order according to the dependency among them.
    May raise [ConfigError(_)] or [PackageError(_)]. *)
