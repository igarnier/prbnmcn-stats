let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:"gaussian"
       ~count:10
       QCheck.(
         triple
           (float_range 0.1 10.)
           (float_range 0.1 10.)
           (float_range 0.1 10.))
  @@ fun (mean, std, x) ->
  let ln_version = exp (Pdfs.gaussian_ln ~mean ~std x) in
  let exp_version = Pdfs.gaussian ~mean ~std x in
  abs_float (ln_version -. exp_version) <=. Float.epsilon

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:"exponential"
       ~count:10
       QCheck.(pair (float_range 0.1 10.) (float_range 0.1 10.))
  @@ fun (rate, x) ->
  let ln_version = exp (Pdfs.exponential_ln ~rate x) in
  let exp_version = Pdfs.exponential ~rate x in
  abs_float (ln_version -. exp_version) <=. Float.epsilon

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:"geometric"
       ~count:10
       QCheck.(pair (int_range 1 10) (float_range 0.1 1.))
  @@ fun (k, p) ->
  let ln_version = exp (Pdfs.geometric_ln ~p k) in
  let exp_version = Pdfs.geometric ~p k in
  abs_float (ln_version -. exp_version) <=. Float.epsilon

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:"poisson"
       ~count:10
       QCheck.(pair (int_range 1 10) (float_range 0.1 10.))
  @@ fun (k, lambda) ->
  let ln_version = exp (Pdfs.poisson_ln ~lambda ~k) in
  let exp_version = Pdfs.poisson ~lambda ~k in
  abs_float (ln_version -. exp_version) <=. Float.epsilon

let () =
  QCheck.Test.check_exn
  @@ QCheck.Test.make
       ~name:"uniform"
       ~count:10
       QCheck.(
         triple (float_range 0.1 10.) (float_range 0.1 10.) (float_range 0.0 1.))
  @@ fun (min, delta, rel_x) ->
  let max = min +. delta in
  let x = min +. ((max -. min) *. rel_x) in
  let ln_version = exp (Pdfs.uniform_ln { min; max } x) in
  let exp_version = Pdfs.uniform { min; max } x in
  abs_float (ln_version -. exp_version) <=. Float.epsilon
