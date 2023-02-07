(** Probability density functions. *)

let poisson ~lambda ~k =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson"
  else
    let log_lambda = log lambda in
    exp @@ ((float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k)

let poisson_ln ~lambda ~k =
  if lambda <=. 0.0 then invalid_arg "Pdfs.poisson_ln"
  else
    let log_lambda = log lambda in
    (float_of_int k *. log_lambda) -. lambda -. Specfun.log_factorial k

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
  normalizer -. (0.5 *. delta_sq)

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

(** [binomial_ln ~p ~n ~k] gives the log-probability of having [k] successes in
    [n] independent Bernouilli trials with probability [p].
    @raise Invalid_argument if [p] is not in the [\[0;1\]] interval,
    if [n < 0], if [k < 0] or if [k > n].
*)
let binomial_ln ~p ~n ~k =
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

(** [binomial_ln ~p ~n ~k] gives the probability of having [k] successes in
    [n] independent Bernouilli trials with probability [p].
    @raise Invalid_argument if [p] is not in the [\[0;1\]] interval,
    if [n < 0], if [k < 0] or if [k > n].
*)
let binomial ~p ~n ~k = exp (binomial_ln ~p ~n ~k)
