(** Binning finitely supported distributions *)

(** The type of binning specifications (shape of the grid, etc) *)
type spec

(** [regular ~origin ~width ~truncate] specifies a regular grid with cells
    aligned on [origin] and spaced by [width]. Cells are indexed,
    with the cell [[origin, origin+width]] having index 0.
    If [truncate] is equal to [Some (l, r)], the points outside of the
    (inclusive) [[l, r]] interval will be discarded.

    @raise Invalid_argument if [width <= 0] or if [truncate = Some (l, r)]and
    the [[l, r]] interval is empty. *)
val regular :
  origin:float -> width:float -> truncate:(float * float) option -> spec

(** [from_measure spec mes] bins the mesure on the grid specified by [spec].
    After binning, the measure is supported by cell indices corresponding
    to the specification. *)
val from_measure : spec -> float Fin.Float.mes -> int Fin.Float.mes

(** [from_empirical spec arr] bins the array on the grid specified by [spec].
    After binning, the measure is supported by cell indices corresponding
    to the specification. *)
val from_empirical : spec -> float Emp.t -> int Fin.Float.mes

(** [map_to_bin spec x] maps [x] to its cell as specified by [spec].
    Returns [None] if the point is out of the range of [spec]. *)
val map_to_bin : spec -> float -> int option

(** [map_from_bin spec i] maps the cell index [i] to the starting
    position of its cell.

    Returns [None] if the index is out of the range of [spec]. *)
val map_from_bin : spec -> int -> float option
