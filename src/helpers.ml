let rec fact (x : Z.t) =
  if Z.equal x Z.zero then Z.one
  else if Z.equal x Z.one then Z.one
  else Z.mul x (fact (Z.pred x))

module Int_table = Hashtbl.Make (struct
  type t = int

  let hash = Hashtbl.hash

  let equal = Int.equal
end)

module String_table = Hashtbl.Make (struct
  type t = string

  let hash = Hashtbl.hash

  let equal = String.equal
end)

module Float_table = Hashtbl.Make (struct
  type t = float

  let hash = Hashtbl.hash

  let equal = Float.equal
end)

let int_table : (module Hashtbl.S with type key = int) = (module Int_table)

let string_table : (module Hashtbl.S with type key = string) =
  (module String_table)

let float_table : (module Hashtbl.S with type key = float) =
  (module Float_table)
