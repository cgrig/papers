open Format

type type_ = (string * type_ list) list
type term = string * term list

let rec nat : type_= ["zero", []; "succ", [nat]; "add", [nat; nat]]
let rec even : type_ =
  ["zero", []; "succ", [odd]; "add", [even; even]; "add", [odd; odd]]
and odd : type_ =
  ["succ", [even]; "add", [odd; even]; "add", [even; odd]]

let zero : term = "zero", []
let one : term = "succ", [zero]
let two : term = "succ", [one]
let three : term = "succ", [two]
let three' : term = "add", [one; two]

let rec check v ts =
  let f (v, vs) (t, ts) =
    v = t &&
    (try List.for_all2 check vs ts with Invalid_argument _ -> false) in
  List.exists (f v) ts

let p v t = printf "@[%b@." (check v t)

let () =
  p one nat;
  p one even;
  p one odd
