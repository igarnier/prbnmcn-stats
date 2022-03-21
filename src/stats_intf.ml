(** Type signatures. *)

(* ------------------------------------------------------------------------- *)

(** The type of (inclusive) ranges. *)
type range = { min : float; max : float }

(* ------------------------------------------------------------------------- *)

(** An ['a iterator] abstracts a finite sequence of elements. *)
type 'a iterator = ('a -> unit) -> unit

(* ------------------------------------------------------------------------- *)

(** Primitive {e representations} of distributions: empirical, generative or
    finitely supported. *)

(** ['a emp] is the type of empirical measures *)
type 'a emp = 'a array

(** ['a gen] is the type of mutable samplers of type ['a] with state of type ['s] *)
type ('s, 'a) gen = 's -> 'a

type ('a, 'r) fin_fun = ('a iterator, 'a, 'r) Linalg.vec

(** [fin_mes] is the type of finitely supported measures. *)
type ('a, 'r) fin_mes = M of { total_mass : 'r; fn : ('a, 'r) fin_fun }

(** [fin_prb] is the type of finitely supported probability measures. *)
type ('a, 'r) fin_prb = P of { fn : ('a, 'r) fin_fun }

(* ------------------------------------------------------------------------- *)

(** PRNG interface, subset of {!Random.State} and [Pringo]. *)
module type Stateful_PRNG = sig
  type t

  val float : t -> float -> float

  val int : t -> int -> int

  val bool : t -> bool
end

(* ------------------------------------------------------------------------- *)

(** Type classes. *)

(** [Gen] allows to manipulate generative probabilities (ie samplers). *)
module type Gen = sig
  type state

  (** Follows the module type of a sampling-based monad *)
  include
    Basic_intf.Monad
      with type 'a t = (state, 'a) gen
       and type 'a res = (state, 'a) gen

  (** [float bound] samples uniformly in [0; bound] *)
  val float : float -> float t

  (** [int bound] samples uniformly in [0; bound-1] *)
  val int : int -> int t

  (** [bool] samples a boolean uniformly *)
  val bool : bool t

  (** [uniform elts] samples an element of the [elts] array by sampling an index uniformly.

      @raise Invalid_argument if [elts] has length equal to 0 *)
  val uniform : 'a array -> 'a t

  (** [bernoulli alpha] samples [true] with probability [alpha].

      @raise Invalid_argument if [alpha] is not in the [0;1] interval. *)
  val bernoulli : float -> bool t

  (** [geometric p] samples a nonnegative integer according to the geometric law of parameter [p].

      @raise Invalid_argument if [p <= 0 || p > 1] *)
  val geometric : float -> int t

  (** [subsample ~n gen] samples one out of [n] samples from [gen]. *)
  val subsample : n:int -> 'a t -> 'a t

  (** [of_empirical emp] samples from [emp] matching exactly the empirical frequencies. *)
  val of_empirical : 'a emp -> 'a t

  (** Exponential distribution via inverse CDF.

      @raise Invalid_argument if [rate <=. 0.0]. *)
  val exponential : rate:float -> float t

  (** Gaussian distribution via Box-Muller transform.
      Returns a pair of {e independent} gaussian variates with
      prescribed mean and standard deviation.

      @raise Invalid_argument if [std <= 0.0] *)
  val box_muller : mean:float -> std:float -> (float * float) t

  (** Gaussian distribution (wrapper over box-muller transform).

      @raise Invalid_argument if [std <= 0.0] *)
  val gaussian : mean:float -> std:float -> float t

  (** Poisson distribution via inverse transform. Consider using other methods
      for large [lambda].

      @raise Invalid_argument if [lambda <= 0.0] *)
  val poisson : lambda:float -> int t

  (** Samples uniformly in the given [range].

      @raise Invalid_argument if [range] is empty. *)
  val range : range -> float t

  (** Gamma distribution. *)
  val gamma : shape:float -> scale:float -> float t

  (** Categorical distribution. Total mass need not be one. Does not aggregate mass of equal elements.

      @raise Invalid_argument if some weights are negative or if the total mass is zero. *)
  val categorical : ('a * float) array -> 'a t

  (** [sample_without_replacement n l] samples a subset of size [n] from [l] without replacement.

      @raise Invalid_argument if [n > List.length l]. *)
  val without_replacement : int -> 'a list -> ('a list * 'a list) t

  module Rational : sig
    (** Categorical distribution. Total mass need not be one. Does not aggregate mass of equal elements.

      @raise Invalid_argument if some weights are negative or if the total mass is zero. *)
    val categorical : ('a * Q.t) array -> 'a t
  end
end

module type Fin_dist = sig
  type state

  type r

  (** The type of finite probability measures with domain ['a] and range [r]. *)
  type 'a prb = ('a, r) fin_prb

  (** The type of finite measures with domain ['a] and range [r]. *)
  type 'a mes = ('a, r) fin_mes

  (** {2:finite_generic Constructing [r]-valued finitely supported functions.} *)

  (** [from_fun len f] constructs a finite function with support [0; ...; len-1].

      @raise Invalid_argument if [len < 0]
   *)
  val of_fun : int -> (int -> r) -> (int, r) fin_fun

  (** [from_array a] returns a finite function wrapping the array [a]. *)
  val of_array : r array -> (int, r) fin_fun

  (** [from_assoc h a] returns a finite function constructed from the bindings in [a].
      The finite function is backed by a hash table whose implementation is given
      through the first class module [h]. The behaviour of the function if elements
      in the support appear more than once is unspecified. *)
  val of_assoc :
    (module Hashtbl.S with type key = 'k) -> ('k * r) array -> ('k, r) fin_fun

  (** Constructing measures and probabilities *)

  (** Creates a finitely supported {e measure} from a finite function.
      A measure is not necessarily normalized.

      @raise Invalid_argument if a point as negative mass. *)
  val measure : ('a, r) fin_fun -> 'a mes

  (** Creates a finitely supported {e probability} from a finite function.
      A probability is normalized.

      @raise Invalid_argument if a point as negative mass or if the total mass does not sum up to one. *)
  val probability : ('a, r) fin_fun -> 'a prb

  (** Forgetful map from the type of finite probabilities to the type of measures. *)
  val as_measure : 'a prb -> 'a mes

  (** Normalize a measure to obtain a probability measure.

      @raise Invalid_argument if the measure has zero mass. *)
  val normalize : 't mes -> 't prb

  (** Computes the empirical measure of an array of elements. Each element
      present in the array is mapped to its count. *)
  val counts_of_empirical :
    (module Hashtbl.S with type key = 't) -> 't array -> 't mes

  (** Finitely supported uniform distribution. *)
  val uniform : 't array -> 't prb

  (** Biased coin. Raises an error if [bias] is not in [0,1]. *)
  val coin : bias:r -> bool prb

  (** Binomial distribution.
      [binomial p n] returns the probability of having
      [k] successes over [n] experiments, according to
      a biased coin [p]. *)
  val binomial : bool prb -> int -> int prb

  (** Using measures and probabilities *)

  (** Integrates a function against a finitely supported measure. *)
  val integrate : 't mes -> ('t -> r) -> r

  (** Evaluates a finitely supported probability on argument. Returns 0 if
      the argument is out of the support. *)
  val eval_prb : 't prb -> 't -> r

  (** Evaluates a finitely supported measure on argument. Returns 0 if
      the argument is out of the support. *)
  val eval_mes : 't mes -> 't -> r

  (** Iterates the given function on the support of the probability. *)
  val iter_prb : 't prb -> ('t -> r -> unit) -> unit

  (** Iterates the given function on the support of the measure. *)
  val iter_mes : 't mes -> ('t -> r -> unit) -> unit

  (** Samples from a finitely supported distribution presented as an unnormalized
      measure. This is mostly useful when sampling only once or twice from a
      distribution: consider converting to a categorical sampler when sampling
      repeatedly. Complexity: O(n) with [n] the cardinality of the support. *)
  val sample : 't mes -> (state, 't) gen

  (** Returns the total mass associated to a finitely supported measure. *)
  val total_mass : 't mes -> r

  (** Compute the mean of a finite measure supported on an [Intf.Module].*)
  val mean_generic :
    (module Basic_intf.Module with type t = 't and type R.t = r) -> 't mes -> 't

  (** Compute the mean of a finite measure supported by r. *)
  val mean : r mes -> r

  (** Compute the variance of a finite measure supported by r. *)
  val variance : r mes -> r

  (** [quantile ord mes p] computes the [p]th quantile of [mes].
      The underlying data is totally ordered by [ord].

      @raise Invalid_argument if [p < 0 || p > 1] or if the total mass of [mes] is zero *)
  val quantile :
    (module Basic_intf.Ordered with type t = 'elt) -> 'elt mes -> r -> 'elt

  (** Returns the raw data underlying a finitely supported measure. *)
  val list_of_measure : 't mes -> [> `Measure of ('t * r) list ]

  (** Returns the raw data underlying a finitely supported probability. *)
  val list_of_probability : 't prb -> [> `Probability of ('t * r) list ]

  type 'a pp := Format.formatter -> 'a -> unit

  (** Pretty print a measure, with elements sorted according to
      the order relation on the support. *)
  val pp_fin_mes : 'a pp -> 'a mes pp

  (** Pretty print a measure, with elements sorted by increasing measure.. *)
  val pp_fin_mes_by_measure : 'a pp -> 'a mes pp

  (** Fold over the union of the supports of the given measures. *)
  val fold_union :
    (module Hashtbl.S with type key = 't) ->
    ('t -> r -> r -> 'a -> 'a) ->
    't mes ->
    't mes ->
    'a ->
    'a
end

(** We use an OCamlgraph-compatible module type to describe
    undirected graphs. We assume that all graphs are undirected
    and simple. *)
module type Graph = sig
  type t

  module V : Basic_intf.Std

  type vertex = V.t

  type edge

  val nb_vertex : t -> int

  val nb_edges : t -> int

  val out_degree : t -> vertex -> int

  val mem_vertex : t -> vertex -> bool

  val mem_edge : t -> vertex -> vertex -> bool

  val succ : t -> vertex -> vertex list

  val succ_e : t -> vertex -> edge list

  val iter_vertex : (vertex -> unit) -> t -> unit

  val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_edges : (vertex -> vertex -> unit) -> t -> unit

  val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a

  val iter_succ : (vertex -> unit) -> t -> vertex -> unit

  val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
end
