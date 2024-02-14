(** Probability density functions (pdfs). *)

(** [poisson ~lambda k] is the value of the pdf of the Poisson distribution of parameter [lambda] at the value [k].

    @raise Invalid_argument if [lambda <= 0.0] *)
val poisson : lambda:float -> int -> float

(** The log-pdf of the Poisson distribution. See {!poisson}. *)
val poisson_ln : lambda:float -> int -> Log_space.t

(** The pdf of the normal, aka gaussian, distribution.

    @raise Invalid_argument if [std <= 0.0] *)
val gaussian : mean:float -> std:float -> float -> float

(** The log-pdf of the normal distribution. See {!gaussian}. *)
val gaussian_ln : mean:float -> std:float -> float -> Log_space.t

(** The pdf of the exponential distribution. *)
val exponential : rate:float -> float -> float

(** The log-pdf of the exponential distribution. *)
val exponential_ln : rate:float -> float -> Log_space.t

(** [geometric ~p k] is the value of pdf of the geometric distribution evaluated at [k].

    @raise Invalid_argument if [p <= 0.0] or [p > 1.0] or [k < 0]. *)
val geometric : p:float -> int -> float

(** [geometric_ln] is the log-pdf of the geometric distribution. See {!geometric}. *)
val geometric_ln : p:float -> int -> Log_space.t

(** [uniform {min; max} x] is the value of the pdf of the uniform distribution on the interval
    [\[min;max\]]. Note that this may evaluate to {!Float.infinity} if [min = max].

    @raise Invalid_argument if [max < min]. *)
val uniform : Stats_intf.range -> float -> float

(** The log-pdf of the uniform distribution. See {!uniform}. *)
val uniform_ln : Stats_intf.range -> float -> Log_space.t

(** [rectangle ~min ~max x] computes the density of [x] of the uniform distribution on
    the axis-aligned box defined by [min] and [max]. Be careful: numerical precision issues
    might arise if the box is too large or has too many dimensions. Use {!rectangle_ln}
    instead if you can.

     @raise Invalid_argument if [Array.length min != Array.length max], if the length of [min]
       or [max] is zero or if there exists an index [i] such that [min.(i) > max.(i)]. *)
val rectangle : min:float array -> max:float array -> float array -> float

(** The log-pdf of the n-dimensional uniform distribution on an axis-aligned box.
    See {!rectangle}. *)
val rectangle_ln :
  min:float array -> max:float array -> float array -> Log_space.t

(** [binomial ~p ~n k] gives the probability of having [k] successes in
    [n] independent Bernouilli trials with probability [p].

    @raise Invalid_argument if [p] is not in the [\[0;1\]] interval,
      if [n < 0], if [k < 0] or if [k > n]. *)
val binomial : p:float -> n:int -> int -> float

(** The log-probability of having [k] successes in
    [n] independent Bernouilli trials with probability [p]. See {!binomial}. *)
val binomial_ln : p:float -> n:int -> int -> Log_space.t

(** [gamma ~shape ~scale x] is the pdf of the gamma distribution.

    @raise Invalid_argument if [shape < 0] or [scale < 0]. *)
val gamma : shape:int -> scale:float -> float -> float

(** [gamma_ln] is the log-pdf of the gamma distribution. See {!gamma}. *)
val gamma_ln : shape:int -> scale:float -> float -> Log_space.t

(** Pdf combinator for random variables with [n] independent components, [n] = 2. *)
val tup2 : ('a -> float) -> ('b -> float) -> 'a * 'b -> float

(** Pdf combinator for random variables with [n] independent components, [n] = 3. *)
val tup3 :
  ('a -> float) -> ('b -> float) -> ('c -> float) -> 'a * 'b * 'c -> float

(** Pdf combinator for random variables with [n] independent components, [n] = 4. *)
val tup4 :
  ('a -> float) ->
  ('b -> float) ->
  ('c -> float) ->
  ('d -> float) ->
  'a * 'b * 'c * 'd ->
  float

(** Pdf combinator for random variables with [n] independent components, [n] = 5. *)
val tup5 :
  ('a -> float) ->
  ('b -> float) ->
  ('c -> float) ->
  ('d -> float) ->
  ('e -> float) ->
  'a * 'b * 'c * 'd * 'e ->
  float

(** Pdf combinator for random variables with [n] independent components, [n] = 6. *)
val tup6 :
  ('a -> float) ->
  ('b -> float) ->
  ('c -> float) ->
  ('d -> float) ->
  ('e -> float) ->
  ('f -> float) ->
  'a * 'b * 'c * 'd * 'e * 'f ->
  float

(** log-pdf combinator for random variables with [n] independent components, [n] = 2. *)
val tup2_ln :
  ('a -> Log_space.t) -> ('b -> Log_space.t) -> 'a * 'b -> Log_space.t

(** log-pdf combinator for random variables with [n] independent components, [n] = 3. *)
val tup3_ln :
  ('a -> Log_space.t) ->
  ('b -> Log_space.t) ->
  ('c -> Log_space.t) ->
  'a * 'b * 'c ->
  Log_space.t

(** log-pdf combinator for random variables with [n] independent components, [n] = 4. *)
val tup4_ln :
  ('a -> Log_space.t) ->
  ('b -> Log_space.t) ->
  ('c -> Log_space.t) ->
  ('d -> Log_space.t) ->
  'a * 'b * 'c * 'd ->
  Log_space.t

(** log-pdf combinator for random variables with [n] independent components, [n] = 5. *)
val tup5_ln :
  ('a -> Log_space.t) ->
  ('b -> Log_space.t) ->
  ('c -> Log_space.t) ->
  ('d -> Log_space.t) ->
  ('e -> Log_space.t) ->
  'a * 'b * 'c * 'd * 'e ->
  Log_space.t

(** log-pdf combinator for random variables with [n] independent components, [n] = 6. *)
val tup6_ln :
  ('a -> Log_space.t) ->
  ('b -> Log_space.t) ->
  ('c -> Log_space.t) ->
  ('d -> Log_space.t) ->
  ('e -> Log_space.t) ->
  ('f -> Log_space.t) ->
  'a * 'b * 'c * 'd * 'e * 'f ->
  Log_space.t

(** [mixture weights pdfs] is the pdf of the convex combination of [pdfs].
    [weights] need to be normalized, non-negative floats.

    @raise Invalid_argument if [mixture] is empty or if [weights] contains a negative weight. *)
val mixture : float array -> (int -> 'a -> float) -> 'a -> float

(** [mixture_ln weights log_pdfs] is the log-pdf of the convex combination of [log_pdfs].
    [weights] need to be normalized, non-negative floats.

    @raise Invalid_argument if [mixture] is empty or if [weights] contains a negative weight. *)
val mixture_ln : float array -> (int -> 'a -> Log_space.t) -> 'a -> Log_space.t
