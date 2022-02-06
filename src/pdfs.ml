(** Probability density functions. *)

let poisson ~lambda =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson"
  else
    let log_lambda = log lambda in
    fun ~k ->
      exp
      @@ ((float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k)

let poisson_ln ~lambda =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson_ln"
  else
    let log_lambda = log lambda in
    fun ~k ->
      (float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k

let pi = acos ~-.1.0

let gaussian ~mean ~std =
  if std <=. 0.0 then invalid_arg "Pdfs.gaussian" ;
  let normalizer = 1.0 /. (std *. (sqrt @@ (2.0 *. pi))) in
  fun x ->
    let delta = ((x -. mean) /. std) ** 2.0 in
    normalizer *. (exp @@ (~-.0.5 *. delta))

let gaussian_ln ~mean ~std =
  if std <=. 0.0 then invalid_arg "Pdfs.gaussian_ln" ;
  let normalizer = -.log std -. (0.5 *. log (2. *. pi)) in
  fun x ->
    let delta = ((x -. mean) /. std) ** 2.0 in
    normalizer -. (0.5 *. delta)

let exponential ~rate x = rate *. exp (~-.rate *. x)

let exponential_ln ~rate x = log rate -. (rate *. x)

let geometric ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.geometric" ;
  if k < 0 then invalid_arg "Pdfs.geometric" ;
  ((1. -. p) ** float_of_int k) *. p

let geometric_ln ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.geometric_ln" ;
  if k < 0 then invalid_arg "Pdfs.geometric_ln" ;
  log p +. (float_of_int k *. log (1. -. p))

let log_geometric ~p k =
  if p <=. 0.0 || p >. 1.0 then invalid_arg "Pdfs.log_geometric" ;
  if k < 0 then invalid_arg "Pdfs.log_geometric" ;
  (log (1. -. p) *. float_of_int k) +. log p

let uniform { Stats_intf.min; max } (x : float) =
  let delta = max -. min in
  if delta >=. 0. then if x >=. min && x <=. max then 1. /. delta else 0.0
  else invalid_arg "uniform"

let uniform_ln { Stats_intf.min; max } (x : float) =
  let delta = max -. min in
  if delta >=. 0. then
    if x >=. min && x <=. max then ~-.(log delta) else neg_infinity
  else invalid_arg "uniform_ln"
