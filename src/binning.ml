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
  [(truncate shifted_reduced, 1.0)]
  [@@inline]

let map { iwidth; origin; truncate_mode } =
  match truncate_mode with
  | None ->
      Fin.Float.kernel
        (module Basic_impl.Free_module.Float_valued.Int)
        (fun x -> map_to_bin origin iwidth x)
  | Some (l, r) ->
      Fin.Float.kernel
        (module Basic_impl.Free_module.Float_valued.Int)
        (fun x -> if x <. l || x >. r then [] else map_to_bin origin iwidth x)

let compute spec (mu : float Fin.Float.mes) : int Fin.Float.mes =
  Fin.Float.pushforward mu (map spec)
