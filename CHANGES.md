## 0.0.8
- Add `Gen.mixture`
- Add `Gen.rectangle`
- Add `Gen.tup[2..6]` combinators
- Add `Pdfs.mixture`
- Add `Pdfs.rectangle` and log variants
- Add `Pdfs.tup[2..6]` combinators and log variants

## 0.0.7
- Add `Pdfs.gamma_ln`
- Add `Gen.shuffle`
- Remove `Pdfs.log_geometric`, use `Pdfs.geometric_ln` instead

## 0.0.6
- Add `Gen.iid`
- In `Emp`: empirical mean, variance return 0 on empty array instead of raising `Invalid_argument`
- Add `Graph.Dist.pp`
- Add sampler for uniform spanning trees in `Graph`
- Remove dependency on `prbnmcn-linalg`

## 0.0.5
- Fix bug in `Fin.eval_prb`
- Fix bug in `Emp.quantile`
- Fix bug in `Graph.diameter`
- Add `Graph.cut`

## 0.0.4
- Expose `Fin.quantile`
- Expose `Fin.iter_prb, Fin.iter_mes`
- `Gen.categorical` takes an array instead of a list
- `Gen`: trivialize `run`
- Refactor/simplification of `Fin`
- Expose `Binning.map_to_bin, Binning.map_from_bin`
- Expose `Binning.from_empirical`
- Rename `Binning.compute` to `Binning.from_measure`
- Typo: `bernouilli` -> `bernoulli`


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
