(** Finite measures implemented as finitely supported functions. *)

(** {1:finite Finitely supported distributions.} *)

(** {2:finite_field_specific Functions specific to the underlying field.} *)

(** [float]-valued finitely supported distributions.  *)
module Float : sig
  include
    Stats_intf.Fin_dist with type r = float and type state = Random.State.t

  (** Distances and divergences between measures. *)
  module Dist : sig
    (** Kullbacak-Leibler divergence. Note that this will diverge if the two measures
        do not have the same support. *)
    val kl : (module Hashtbl.S with type key = 'a) -> 'a mes -> 'a mes -> float

    (** Lp distance.

        @raise Invalid_argument if [p < 1] *)
    val lp :
      (module Hashtbl.S with type key = 'a) -> p:r -> 'a mes -> 'a mes -> r

    (** L-infinity distance *)
    val linf : (module Hashtbl.S with type key = 'a) -> 'a mes -> 'a mes -> r
  end
end

(** [Q]-valued finitely supported distributions.  *)
module Rational : sig
  include Stats_intf.Fin_dist with type r = Q.t and type state = Random.State.t

  module Dist : sig
    (** L-infinity distance *)
    val linf : (module Hashtbl.S with type key = 'a) -> 'a mes -> 'a mes -> r
  end
end
