
open MyUtil
open Errors

exception PackageError of package_error

val main : absolute_dir -> (absolute_path * ConfigLoader.config) list
