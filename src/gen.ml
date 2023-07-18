module Make (RNG : sig
  type t

  val float : t -> float -> float

  val int : t -> int -> int

  val bool : t -> bool
end) =
struct
  module M :
    Basic_intf.Monad
      with type 'a t = (RNG.t, 'a) Stats_intf.gen
       and type 'a res = (RNG.t, 'a) Stats_intf.gen = struct
    type 'a t = (RNG.t, 'a) Stats_intf.gen

    type 'a res = (RNG.t, 'a) Stats_intf.gen

    let bind m f state =
      let x = m state in
      f x state

    let map m f state =
      let x = m state in
      f x

    let return x _state = x

    let run = Fun.id

    module Infix = struct
      let ( >>= ) = bind

      let ( >|= ) = map

      let ( let* ) = bind

      let return = return
    end
  end

  include M

  type state = RNG.t

  let iid (gen : 'a t) state =
    Seq.unfold
      (fun state ->
        let res = gen state in
        Some (res, state))
      state

  let float bound state = RNG.float state bound

  let int bound state = RNG.int state bound

  let bool = RNG.bool

  let range { Stats_intf.min; max } state =
    if max -. min >=. 0. then min +. RNG.float state (max -. min)
    else invalid_arg "uniform_in_interval"

  let _bernoulli h state =
    let x = RNG.float state 1.0 in
    x <. h

  let bernoulli h state =
    if h <. 0.0 || h >. 1.0 then invalid_arg "bernoulli" ;
    _bernoulli h state

  let geometric p state =
    if p <=. 0.0 || p >. 1.0 then invalid_arg "geometric" ;
    let failures = ref 0 in
    while not (_bernoulli p state) do
      incr failures
    done ;
    !failures

  let shuffle (elts : 'a array) state =
    let len = Array.length elts in
    let p = Array.copy elts in
    for i = len downto 2 do
      let m = RNG.int state i in
      let n' = i - 1 in
      if m <> n' then (
        let tmp = p.(m) in
        p.(m) <- p.(n') ;
        p.(n') <- tmp)
    done ;
    p

  let uniform (elts : 'a array) =
    let len = Array.length elts in
    if len = 0 then invalid_arg "uniform" ;
    fun state ->
      let i = int len state in
      elts.(i)

  let subsample ~n sampler : 'a t =
   fun rng_state ->
    let counter = ref 0 in
    let rec loop rng_state =
      let res = sampler rng_state in
      incr counter ;
      if Int.equal (!counter mod n) 0 then res else loop rng_state
    in
    loop rng_state

  let of_empirical : 'a Stats_intf.emp -> 'a t =
   fun data rng_state ->
    let len = Array.length data in
    if len = 0 then invalid_arg "of_empirical: length of data = 0" ;
    let i = RNG.int rng_state len in
    data.(i)

  module Float = struct
    let exponential ~rate : float t =
     fun rng_state ->
      if rate <=. 0.0 then invalid_arg "exponential: rate <= 0" ;
      let u = RNG.float rng_state 1.0 in
      ~-.(log u) /. rate

    let box_muller : mean:float -> std:float -> (float * float) t =
      let rec reject_loop rng_state =
        let u = RNG.float rng_state 2.0 -. 1.0 in
        let v = RNG.float rng_state 2.0 -. 1.0 in
        let s = (u *. u) +. (v *. v) in
        if s =. 0.0 || s >=. 1.0 then reject_loop rng_state
        else
          let weight = sqrt (-2. *. log s /. s) in
          let variate1 = u *. weight in
          let variate2 = v *. weight in
          (variate1, variate2)
      in
      fun ~mean ~std rng_state ->
        if std <=. 0.0 then invalid_arg "box_muller" ;
        let (v1, v2) = reject_loop rng_state in
        (mean +. (std *. v1), mean +. (std *. v2))

    type gaussgen_state = Fresh | Last of float

    let gaussian ~mean ~std : float t =
      if std <=. 0.0 then invalid_arg "gaussian: std <= 0" ;
      let state = ref Fresh in
      let gen = box_muller ~mean ~std in
      fun rng_state ->
        match !state with
        | Fresh ->
            let (x1, x2) = gen rng_state in
            state := Last x2 ;
            x1
        | Last x ->
            state := Fresh ;
            x

    let poisson ~lambda : int t =
     fun rng_state ->
      if lambda <=. 0.0 then invalid_arg "poisson: lambda <= 0" ;
      let rec loop x p s u =
        if u >. s then
          let x = x + 1 in
          let p = p *. lambda /. float_of_int x in
          let s = s +. p in
          loop x p s u
        else x
      in
      let u = RNG.float rng_state 1.0 in
      let p = exp ~-.lambda in
      let s = p in
      let x = 0 in
      loop x p s u

    (*
     * The following code for sampling Gamma RVs is due to the Owl project and is MIT licensed.
     *
     * https://github.com/owlbarn/owl/
     *
     * OWL - OCaml Scientific and Engineering Computing
     * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
     *)

    let std_exponential_rvs state =
      let u = RNG.float state 1. in
      -.log1p (-.u)

    let std_gamma_rvs ~shape state =
      let exception Found in
      let x = ref infinity in
      (if shape =. 1. then x := std_exponential_rvs state
       else if shape <. 1. then
         try
           while true do
             let u = RNG.float state 1. in
             let v = std_exponential_rvs state in
             if u <=. 1. -. shape then (
               x := u ** (1. /. shape) ;
               if !x <=. v then raise Found)
             else
               let y = -.log ((1. -. u) /. shape) in
               x := (1. -. shape +. (shape *. y)) ** (1. /. shape) ;
               if !x <=. v +. y then raise Found
           done
         with _ -> ()
       else
         let b = shape -. (1. /. 3.) in
         let c = 1. /. sqrt (9. *. b) in
         try
           while true do
             let v = ref neg_infinity in
             while !v <=. 0. do
               x := gaussian ~mean:0.0 ~std:1.0 state ;
               v := 1. +. (c *. !x)
             done ;
             let v = !v *. !v *. !v in
             let u = RNG.float state 1. in
             if u <. 1. -. (0.0331 *. !x *. !x *. !x *. !x) then (
               x := b *. v ;
               raise Found) ;
             if log u <. (0.5 *. !x *. !x) +. (b *. (1. -. v +. log v)) then (
               x := b *. v ;
               raise Found)
           done
         with Found -> ()) ;
      !x

    let gamma ~shape ~scale state = scale *. std_gamma_rvs ~shape state

    module Alias_f =
      Alias.Make (Basic_impl.Reals.Float) (Basic_impl.Reals.Float) (M)
        (struct
          type 'a t = 'a M.t

          let mass bound state = RNG.float state bound

          let int bound state = RNG.int state bound
        end)

    let categorical cases =
      let s = Alias_f.create cases in
      fun state -> Alias_f.sampler s state

    let rec take_n n list acc =
      if n = 0 then (List.rev acc, list)
      else
        match list with
        | [] -> invalid_arg "take_n"
        | x :: tl -> take_n (n - 1) tl (x :: acc)

    let without_replacement n list rng_state =
      let (first_n, rest) = take_n n list [] in
      let reservoir = Array.of_list first_n in
      let reject = ref [] in
      List.iteri
        (fun index elt ->
          let i = n + index in
          let j = RNG.int rng_state (i + 1) in
          if j < n then (
            reject := reservoir.(j) :: !reject ;
            reservoir.(j) <- elt)
          else reject := elt :: !reject)
        rest ;
      (Array.to_list reservoir, !reject)
  end

  include Float

  module Rational = struct
    module Alias_q =
      Alias.Make (Basic_impl.Reals.Rational) (Basic_impl.Reals.Rational) (M)
        (struct
          type 'a t = 'a M.t

          let mass bound state =
            let q = Q.of_float (RNG.float state 1.0) in
            Basic_impl.Reals.Rational.(bound * q)

          let int bound state = RNG.int state bound
        end)

    let categorical list =
      let s = Alias_q.create list in
      fun state -> Alias_q.sampler s state
  end
end

include Make (Random.State)
