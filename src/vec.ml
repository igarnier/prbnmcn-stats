(** Type of generic input vectors (vectors from which we {e get} elements) *)
type ('s, 'i, 'e) vec = Vec of 's * ('i -> 'e)

(** Type of generic output vectors (vectors to which we {e set} elements). *)
type ('s, 'i, 'e, 'w) ovec = OVec of 's * ('i -> 'e -> 'w)
