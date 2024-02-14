(** Generative distributions. *)

(** Implementation of generative distributions parameterized by
    stateful RNG implementation. *)
module Make (RNG : Stats_intf.Stateful_PRNG) :
  Stats_intf.Gen with type state = RNG.t

(** Instantiation of the {!Make} functor with {!Random.State}. *)
include Stats_intf.Gen with type state = Random.State.t
