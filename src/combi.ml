(* Simple combinatorics. *)

(* -------------------------------------------------------------------------- *)
(* Enumerate all ways of picking n elements from a list (ie all injections
   from [n] to [l]) *)

let rec enumerate_injections n l current_pick acc =
  if n = 0 then List.rev current_pick :: acc
  else enumerate_picks n l current_pick acc

and enumerate_picks n l current_pick acc =
  match l with
  | [] -> acc
  | x :: tl ->
      let extended_pick = x :: current_pick in
      let acc = enumerate_injections (n - 1) tl extended_pick acc in
      enumerate_picks n tl current_pick acc

let enumerate_subsets n l = enumerate_injections n l [] []
