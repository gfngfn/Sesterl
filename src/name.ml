
type t = string


let local (s : string) : t = s

let global (s : string) : t = s

let unused : t = "_"

let fresh () : t = "fresh"  (* temporary *)
