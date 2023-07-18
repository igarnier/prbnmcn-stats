(* ------------------------------------------------------------------------- *)
(* Generate finite approximations of various dists *)

let rng_state = Random.State.make [| 0x1337; 0x533D; 0xD3AD; 0XB33F |]

let spec = Binning.regular ~origin:0.0 ~width:0.05 ~truncate:(Some (-2., 2.))

let empirical_gaussian =
  Emp.of_generative ~nsamples:1000 (Gen.gaussian ~mean:0.0 ~std:1.0) rng_state

let empirical_exp =
  Emp.of_generative ~nsamples:1000 (Gen.exponential ~rate:1.) rng_state

let binned_gaussian = Binning.from_empirical spec empirical_gaussian

let binned_exp = Binning.from_empirical spec empirical_exp

let empirical_exp' =
  Emp.of_generative ~nsamples:1000 (Gen.exponential ~rate:2.) rng_state

let binned_exp' = Binning.from_empirical spec empirical_exp
