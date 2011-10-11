open Format

type type_ =
  | AT of (string * type_ list) list
  | IT of (int -> bool)
  | ST of (string -> bool)
type term =
  | A of string * term list
  | I of int
  | S of string

let rec nat : type_= AT ["zero", []; "succ", [nat]; "add", [nat; nat]]
let rec even : type_ =
  AT ["zero", []; "succ", [odd]; "add", [even; even]; "add", [odd; odd]]
and odd : type_ =
  AT ["succ", [even]; "add", [odd; even]; "add", [even; odd]]

let zero : term = A ("zero", [])
let one : term = A ("succ", [zero])
let two : term = A ("succ", [one])
let three : term = A ("succ", [two])
let three' : term = A ("add", [one; two])

let all2 f xs ys =
  try List.for_all2 f xs ys
  with Invalid_argument _ -> false

let rec check v ts = match (v, ts) with
  | A (v, vs), AT ts ->
      List.exists (fun (t, ts) -> v = t && all2 check vs ts) ts
  | I v, IT ts -> ts v
  | S v, ST ts -> ts v
  | _ -> false

let p v t = printf "@[%b@." (check v t)

let () =
  p one nat;
  p one even;
  p one odd
