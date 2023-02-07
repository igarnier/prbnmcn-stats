type spec =
  { iwidth : float;  (** inverse width *)
    origin : float;  (** start of the first cell *)
    truncate_mode : (float * float) option
  }

let regular ~origin ~width ~truncate =
  if width <=. 0.0 then invalid_arg "spec" ;
  (match truncate with
  | Some (l, r) when l >=. r -> invalid_arg "regular"
  | _ -> ()) ;
  { iwidth = 1. /. width; origin; truncate_mode = truncate }

let map_to_bin origin iwidth x =
  let shifted_reduced = floor @@ ((x -. origin) *. iwidth) in
  truncate shifted_reduced
  [@@inline]

let map_to_bin { iwidth; origin; truncate_mode } x =
  match truncate_mode with
  | None -> Some (map_to_bin origin iwidth x)
  | Some (l, r) ->
      if x <. l || x >. r then None else Some (map_to_bin origin iwidth x)

let map_from_bin { iwidth; origin; truncate_mode } index =
  let x = origin +. (float_of_int index /. iwidth) in
  match truncate_mode with
  | None -> Some x
  | Some (l, r) -> if x <. l || x >. r then None else Some x

(* TODO tests *)
let from_measure spec (M { fn; total_mass = _ } : float Fin.Float.mes) :
    int Fin.Float.mes =
  let (Vec.Vec (iter, f)) = fn in
  let bins = Helpers.Int_table.create 100 in
  iter (fun x ->
      match map_to_bin spec x with
      | None -> ()
      | Some bin -> (
          match Helpers.Int_table.find_opt bins bin with
          | None -> Helpers.Int_table.add bins bin (f x)
          | Some count -> Helpers.Int_table.replace bins bin (count +. f x))) ;
  let finfn =
    let iterator f = Helpers.Int_table.iter (fun k _ -> f k) bins in
    Vec.Vec
      ( iterator,
        fun k -> try Helpers.Int_table.find bins k with Not_found -> 0.0 )
  in
  Fin.Float.(measure finfn)

let from_empirical spec (mu : float array) : int Fin.Float.mes =
  let bins = Helpers.Int_table.create 100 in
  Array.iter
    (fun x ->
      match map_to_bin spec x with
      | None -> ()
      | Some bin -> (
          match Helpers.Int_table.find_opt bins bin with
          | None -> Helpers.Int_table.add bins bin 1.0
          | Some count -> Helpers.Int_table.replace bins bin (count +. 1.)))
    mu ;
  let finfn =
    let iterator f = Helpers.Int_table.iter (fun k _ -> f k) bins in
    Vec.Vec
      ( iterator,
        fun k -> try Helpers.Int_table.find bins k with Not_found -> 0.0 )
  in
  Fin.Float.(measure finfn)
