## 0.0.3
- Preserve underlying order of support in `Fin.raw_data_*`
- Expose implementation of `Gen` module generic over RNG (for Pringo compatibility)
- Expose `Specfun` module with `log_factorial`
- Move sampling without replacement to `Gen`
- Generalize type of `Mh.Make_core_sampling_loop`
- Better error messages for some samplers in `Gen`
- log-space version of some pdfs
- fix binning module
- copy OCaml impl. of gamma sampler from `Owl`

## 0.0.2
- add Log_space.unsafe_cast
- fix documentation of Gen.int
- add poisson sampler
- add range checking for exponential, gaussian sampler parameter

## 0.0.1
- First release
