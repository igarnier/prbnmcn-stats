let poisson ~lambda k =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson"
  else
    let log_lambda = log lambda in
    exp @@ ((float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k)

let poisson_ln ~lambda k =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson_ln"
  else
    let log_lambda = log lambda in
    Log_space.unsafe_cast
      ((float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k)

let pi = acos ~-.1.0

let gaussian ~mean ~std x =
  if std <=. 0.0 then invalid_arg "Pdfs.gaussian" ;
  let normalizer = 1.0 /. (std *. (sqrt @@ (2.0 *. pi))) in
  let delta = (x -. mean) /. std in
  let delta_sq = delta *. delta in
  normalizer *. (exp @@ (~-.0.5 *. delta_sq))

let gaussian_ln ~mean ~std x =
  if std <=. 0.0 then invalid_arg "Pdfs.gaussian_ln" ;
  let normalizer = -.log std -. (0.5 *. log (2. *. pi)) in
  let delta = (x -. mean) /. std in
  let delta_sq = delta *. delta in
  Log_space.unsafe_cast (normalizer -. (0.5 *. delta_sq))

let exponential ~rate x = rate *. exp (~-.rate *. x)

let exponential_ln ~rate x = Log_space.unsafe_cast (log rate -. (rate *. x))

let geometric ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.geometric" ;
  if k < 0 then invalid_arg "Pdfs.geometric" ;
  ((1. -. p) ** float_of_int k) *. p

let geometric_ln ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.geometric_ln" ;
  if k < 0 then invalid_arg "Pdfs.geometric_ln" ;
  Log_space.unsafe_cast (log p +. (float_of_int k *. log (1. -. p)))

let uniform { Stats_intf.min; max } (x : float) =
  let delta = max -. min in
  if delta >=. 0. then if x >=. min && x <=. max then 1. /. delta else 0.0
  else invalid_arg "uniform"

let uniform_ln { Stats_intf.min; max } (x : float) =
  let delta = max -. min in
  if delta >=. 0. then
    if x >=. min && x <=. max then Log_space.unsafe_cast ~-.(log delta)
    else Log_space.zero
  else invalid_arg "uniform_ln"

let rectangle_ln ~min ~max (x : float array) =
  if Array.length min <> Array.length max then
    invalid_arg "rectangle_ln: Array.length min <> Array.length max" ;
  if Array.length min = 0 then invalid_arg "rectangle_ln: min has zero length" ;
  let deltas = Array.map2 (fun x y -> y -. x) min max in
  if Array.exists (fun delta -> delta <. 0.0) deltas then
    invalid_arg "rectangle_ln: invalid box, some dimension is empty" ;
  let exception Out_of_range in
  let acc = ref 0.0 in
  try
    for i = 0 to Array.length min - 1 do
      let xi = x.(i) in
      if xi >=. min.(i) || xi <=. max.(i) then acc := !acc -. log deltas.(i)
      else raise Out_of_range
    done ;
    Log_space.unsafe_cast !acc
  with Out_of_range -> Log_space.zero

let rectangle ~min ~max x = Log_space.to_float (rectangle_ln ~min ~max x)

let binomial_ln ~p ~n k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.binomial_ln" ;
  if n < 0 then invalid_arg "Pdfs.binomial_ln" ;
  if k < 0 || k > n then invalid_arg "Pdfs.binomial_ln" ;
  (*
    log (bincoeff n k) + k log p + (n-k) log (1-p)
    log (bincoeff n k) = log (n!) - (log (k!) + log ((n-k)!))
   *)
  let log_bincoeff =
    let open Specfun in
    log_factorial n -. log_factorial k -. log_factorial (n - k)
  in
  log_bincoeff
  +. (float_of_int k *. log p)
  +. (float_of_int (n - k) *. log (1. -. p))
  |> Log_space.unsafe_cast

let binomial ~p ~n k = Log_space.to_float (binomial_ln ~p ~n k)

let gamma_ln ~shape ~scale x =
  if shape <= 0 then invalid_arg "Pdfs.gamma_ln" ;
  if scale <=. 0.0 then invalid_arg "Pdfs.gamma_ln" ;
  let fshape = float_of_int shape in
  ((fshape -. 1.) *. log x)
  -. (x /. scale)
  -. (fshape *. log scale)
  -. Specfun.log_factorial (shape - 1)
  |> Log_space.unsafe_cast

let gamma ~shape ~scale x = Log_space.to_float (gamma_ln ~shape ~scale x)

let tup2 p1 p2 (a, b) = p1 a *. p2 b

let tup3 p1 p2 p3 (a, b, c) = p1 a *. p2 b *. p3 c

let tup4 p1 p2 p3 p4 (a, b, c, d) = p1 a *. p2 b *. p3 c *. p4 d

let tup5 p1 p2 p3 p4 p5 (a, b, c, d, e) = p1 a *. p2 b *. p3 c *. p4 d *. p5 e

let tup6 p1 p2 p3 p4 p5 p6 (a, b, c, d, e, f) =
  p1 a *. p2 b *. p3 c *. p4 d *. p5 e *. p6 f

let ( *: ) = Log_space.mul

let tup2_ln p1 p2 (a, b) = p1 a *: p2 b

let tup3_ln p1 p2 p3 (a, b, c) = p1 a *: p2 b *: p3 c

let tup4_ln p1 p2 p3 p4 (a, b, c, d) = p1 a *: p2 b *: p3 c *: p4 d

let tup5_ln p1 p2 p3 p4 p5 (a, b, c, d, e) =
  p1 a *: p2 b *: p3 c *: p4 d *: p5 e

let tup6_ln p1 p2 p3 p4 p5 p6 (a, b, c, d, e, f) =
  p1 a *: p2 b *: p3 c *: p4 d *: p5 e *: p6 f

let mixture_ln coeffs log_pdfs =
  if Array.length coeffs = 0 then invalid_arg "mixture_ln: empty coeffs" ;
  let sum = ref 0.0 in
  for i = 0 to Array.length coeffs - 1 do
    let c = coeffs.(i) in
    if c <. 0.0 then invalid_arg "mixture_ln: negative weight" ;
    sum := !sum +. coeffs.(i)
  done ;
  if abs_float (1. -. !sum) >. 0.001 then
    invalid_arg "mixture_ln: unnormalized weights" ;
  let log_coeffs = Array.map Log_space.of_float coeffs in
  fun x ->
    let open Log_space in
    let acc = ref one in
    for i = 0 to Array.length coeffs - 1 do
      acc := mul !acc (mul log_coeffs.(i) (log_pdfs i x))
    done ;
    !acc

let mixture coeffs pdfs x =
  mixture_ln coeffs (fun i x -> Log_space.of_float (pdfs i x)) x
  |> Log_space.to_float
