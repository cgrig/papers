open Format

type type_ = (string * type_ list) list
let rec nat : type_= [("zero", []); ("succ", [nat])]

type term = string * term list
let one : term = "succ", ["zero", []]

let rec check v ts =
  let f (v, vs) (t, ts) = v = t && List.for_all2 check vs ts in
  List.exists (f v) ts

let () = printf "@[%b@." (check one nat)
