(* ------------------------------------------------------------------------- *)

module Make (Reals : Basic_intf.Reals) = struct
  type r = Reals.t

  type 'a finfn = ('a, r) Stats_intf.fin_fun

  type 'a prb = ('a, r) Stats_intf.fin_prb

  type 'a mes = ('a, r) Stats_intf.fin_mes

  type state = Random.State.t

  let of_fun : int -> (int -> r) -> int finfn =
   fun len f ->
    if len < 0 then invalid_arg "of_fun" ;
    let iterator f =
      for i = 0 to len - 1 do
        f i
      done
    in
    Vec.Vec (iterator, fun i -> if i < 0 || i >= len then Reals.zero else f i)

  let of_array : r array -> int finfn =
   fun array ->
    let iterator f =
      for i = 0 to Array.length array - 1 do
        f i
      done
    in
    Vec.Vec
      ( iterator,
        fun i ->
          if i < 0 || i >= Array.length array then Reals.zero
          else Array.unsafe_get array i )

  let of_assoc :
      type k.
      (module Hashtbl.S with type key = k) ->
      (k * r) array ->
      (k, r) Stats_intf.fin_fun =
    fun (type k) (module H : Hashtbl.S with type key = k) array ->
     let table = H.of_seq (Array.to_seq array) in
     let iterator f = H.iter (fun k _ -> f k) table in
     Vec.Vec
       (iterator, fun k -> try H.find table k with Not_found -> Reals.zero)

  let measure : 'a finfn -> 'a mes =
    fun (type a) (finfn : a finfn) ->
     let (Vec.Vec (iter, f)) = finfn in
     let acc = ref Reals.zero in
     iter (fun i ->
         let w = f i in
         acc := Reals.add !acc w ;
         if Reals.(w < zero) then invalid_arg "measure: negative weight") ;
     Stats_intf.M { total_mass = !acc; fn = finfn }

  let probability : 'a finfn -> 'a prb =
    fun (type a) (finfn : a finfn) ->
     let (Vec.Vec (iter, f)) = finfn in
     let acc = ref Reals.zero in
     iter (fun i -> acc := Reals.add !acc (f i)) ;
     let total_mass = !acc in
     if not Reals.(total_mass = Reals.one) then
       Format.kasprintf
         invalid_arg
         "probability: mass do not sum up to 1 (%a)"
         Reals.pp
         total_mass ;
     iter (fun i ->
         if Reals.(f i < zero) then invalid_arg "probability: negative mass") ;
     Stats_intf.P { fn = finfn }

  let as_measure (Stats_intf.P { fn }) =
    Stats_intf.M { total_mass = Reals.one; fn }

  let total_mass (type t) (M { total_mass; _ } : t mes) : r = total_mass

  let normalize (type t) (M { total_mass; fn } : t mes) :
      (t, r) Stats_intf.fin_prb =
    let (Vec.Vec (shape, f)) = fn in
    if Reals.equal total_mass Reals.zero then
      invalid_arg "normalize: null measure" ;
    let inv_w = Reals.(one / total_mass) in
    let fn = Vec.Vec (shape, fun i -> Reals.mul (f i) inv_w) in
    P { fn }

  let sample (type t) (M { total_mass; fn } : t mes) rng_state =
    let exception Sampled of t in
    let (Vec.Vec (iter, f)) = fn in
    let r = Reals.(total_mass * lebesgue rng_state) in
    let cumu = ref Reals.zero in
    try
      let _ =
        iter (fun i ->
            let w = f i in
            let c = Reals.add !cumu w in
            if Reals.(r <= c) then raise (Sampled i) ;
            cumu := c)
      in
      (* should be unreachable *)
      assert false
    with Sampled x -> x

  let counts_of_empirical (type t) (module H : Hashtbl.S with type key = t)
      (p : t array) : t mes =
    let table = H.create (Array.length p) in
    Array.iter
      (fun elt ->
        match H.find_opt table elt with
        | None -> H.add table elt 1
        | Some c -> H.replace table elt (c + 1))
      p ;
    let iter f = H.iter (fun k _ -> f k) table in
    let total_mass = Reals.of_int (Array.length p) in
    let fn =
      Vec.Vec
        ( iter,
          fun k ->
            try Reals.of_int (H.find table k) with Not_found -> Reals.zero )
    in
    M { total_mass; fn }

  let uniform (type t) (arr : t array) : t prb =
    let len = Array.length arr in
    if Int.equal len 0 then failwith "uniform: empty array"
    else
      let prb = Reals.(div one (of_int len)) in
      let iter f =
        for i = 0 to Array.length arr - 1 do
          f arr.(i)
        done
      in
      P { fn = Vec.Vec (iter, fun _elt -> prb) }

  let eval_prb (type t) (P { fn } : t prb) (x : t) : r =
    let (Vec.Vec (_, f)) = fn in
    f x

  let eval_mes (type t) (M { fn; total_mass = _ } : t mes) (x : t) : r =
    let (Vec.Vec (_, f)) = fn in
    f x

  let iter_prb (type t) (P { fn } : t prb) f =
    let (Vec.Vec (iter, p)) = fn in
    iter (fun x -> f x (p x))

  let iter_mes (type t) (M { fn; total_mass = _ } : t mes) f =
    let (Vec.Vec (iter, p)) = fn in
    iter (fun x -> f x (p x))

  let integrate (type t) (M { fn; total_mass = _ } : t mes) (f : t -> r) : r =
    let acc = ref Reals.zero in
    let (Vec.Vec (iter, m)) = fn in
    iter (fun x -> acc := Reals.(!acc + (m x * f x))) ;
    !acc

  let list_of_measure (type t) (M { fn; total_mass = _ } : t mes) =
    let (Vec.Vec (iter, m)) = fn in
    let acc = ref [] in
    iter (fun x -> acc := (x, m x) :: !acc) ;
    `Measure (List.rev !acc)

  let list_of_probability (type t) (P { fn } : t prb) =
    let (Vec.Vec (iter, m)) = fn in
    let acc = ref [] in
    iter (fun x -> acc := (x, m x) :: !acc) ;
    `Probability (List.rev !acc)

  let pp_fin_mes :
      type a.
      (Format.formatter -> a -> unit) ->
      Format.formatter ->
      (a, r) Stats_intf.fin_mes ->
      unit =
   fun pp fmtr den ->
    let (`Measure l) = list_of_measure den in
    Format.fprintf
      fmtr
      "@[<h>%a@]"
      (Format.pp_print_list (fun elt_fmt (elt, pr) ->
           Format.fprintf elt_fmt "(%a, %a);@," pp elt Reals.pp pr))
      l

  let pp_fin_mes_by_measure :
      type a.
      (Format.formatter -> a -> unit) ->
      Format.formatter ->
      (a, r) Stats_intf.fin_mes ->
      unit =
   fun pp fmtr den ->
    let (`Measure l) = list_of_measure den in
    let l = List.sort (fun (_, r1) (_, r2) -> Reals.compare r1 r2) l in
    Format.fprintf
      fmtr
      "@[<h>%a@]"
      (Format.pp_print_list (fun elt_fmt (elt, pr) ->
           Format.fprintf elt_fmt "(%a, %a);@," pp elt Reals.pp pr))
      l

  let coin ~bias : bool prb =
    if Reals.(bias < zero || bias > one) then invalid_arg "coin: invalid bias"
    else
      let compl = Reals.(one - bias) in
      let iter f =
        f true ;
        f false
      in
      let fn = Vec.Vec (iter, function true -> bias | false -> compl) in
      probability fn

  let bincoeff n k =
    let n = Reals.of_int n in
    let rec loop i acc =
      if Int.equal i (k + 1) then acc
      else
        let fi = Reals.of_int i in
        loop (i + 1) Reals.(acc * ((n + one - fi) / fi))
    in
    loop 1 Reals.one

  let binomial (coin : bool prb) n =
    let p = eval_prb coin true in
    let not_p = eval_prb coin false in
    let elements =
      Array.init n (fun k ->
          let n_minus_k = n - k in
          Reals.(k, bincoeff n k * npow p k * npow not_p n_minus_k))
    in
    let finfn = of_assoc (module Helpers.Int_table) elements in
    normalize @@ measure finfn

  let mean_generic (type t)
      (module L : Basic_intf.Module with type t = t and type R.t = Reals.t)
      (M { fn; total_mass = _ } : t mes) =
    let (Vec.Vec (iter, m)) = fn in
    let acc = ref L.zero in
    iter (fun x -> acc := L.add (L.smul (m x) x) !acc) ;
    !acc

  let mean (dist : r mes) = integrate dist Fun.id

  let variance (M { fn; total_mass = _ } as dist : r mes) =
    let (Vec.Vec (iter, m)) = fn in
    let mean = mean dist in
    let acc = ref Reals.zero in
    iter (fun x ->
        let acc' =
          let open Reals in
          let delta = x - mean in
          let delta_squared = delta * delta in
          !acc + (delta_squared * m x)
        in
        acc := acc') ;
    !acc

  let quantile (type elt) (module O : Basic_intf.Ordered with type t = elt)
      (M { fn; total_mass } : elt mes) (p : r) =
    if Reals.(p < zero) || Reals.(p > one) then
      invalid_arg "quantile (invalid p)" ;
    if Reals.(total_mass = Reals.zero) then invalid_arg "quantile (zero mass)" ;
    let p = Reals.(p * total_mass) in
    let (Vec.Vec (iter, m)) = fn in
    let elts = ref [] in
    iter (fun x -> elts := (x, m x) :: !elts) ;
    let arr = Array.of_list !elts in
    Array.sort (fun (x, _) (y, _) -> O.compare x y) arr ;
    let acc = ref Reals.zero in
    (*
       The pth quantile is the first element in [arr] for
       which the cumulative weight is above or equal to [p]
    *)
    let exception Found of elt in
    try
      Array.iter
        (fun (x, q) ->
          (acc := Reals.(!acc + q)) ;
          if Reals.(!acc >= p) then raise (Found x))
        arr ;
      fst arr.(Array.length arr - 1)
    with Found elt -> elt

  let fold_union (type x) (module H : Hashtbl.S with type key = x) f
      (m1 : x mes) (m2 : x mes) acc =
    let (M { fn = Vec.Vec (iter1, m1); total_mass = _ }) = m1 in
    let (M { fn = Vec.Vec (iter2, m2); total_mass = _ }) = m2 in
    let table = H.create 127 in
    iter1 (fun x -> H.add table x (m1 x)) ;
    let acc = ref acc in
    iter2 (fun y ->
        let r = m2 y in
        match H.find_opt table y with
        | None -> acc := f y Reals.zero r !acc
        | Some r' ->
            H.remove table y ;
            acc := f y r r' !acc) ;
    (* process elements in support(m1)\support(m2) *)
    H.fold (fun x r acc -> f x r Reals.zero acc) table !acc
end
[@@inline]

module Float = struct
  include Make (Basic_impl.Reals.Float)

  module Dist = struct
    let kl h m1 m2 =
      fold_union h (fun _ r1 r2 acc -> acc +. (r1 *. log (r1 /. r2))) m1 m2 0.0

    let lp h ~p m1 m2 =
      if p <. 1. then invalid_arg "lp: p < 1" ;
      let res =
        fold_union
          h
          (fun _ r1 r2 acc -> acc +. (abs_float (r1 -. r2) ** p))
          m1
          m2
          0.0
      in
      res ** (1. /. p)

    let maxf x y = if x <. y then y else x

    let linf h m1 m2 =
      fold_union
        h
        (fun _ r1 r2 acc -> maxf acc (abs_float (r1 -. r2)))
        m1
        m2
        0.0
  end
end

module Rational = struct
  include Make (Basic_impl.Reals.Rational)

  module Dist = struct
    let linf h m1 m2 =
      fold_union
        h
        (fun _ r1 r2 acc -> Q.max acc (Q.abs (Q.sub r1 r2)))
        m1
        m2
        Q.zero
  end
end
