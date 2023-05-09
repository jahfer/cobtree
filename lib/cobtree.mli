module Density : sig
  type t = {
    max_n : int;
    range : Q.t * Q.t;
  }

  module type IConfig = sig
    val p_max : Q.t
    val t_min : Q.t
    val p_min : Q.t
    val t_max : Q.t
  end

  module Make : functor (_ : IConfig) ->
    sig
      val ideal_density : Q.t
      val t_delta : Q.t
      val p_delta : Q.t
      val range_list : max_size:int -> t list
    end

  val compare : t -> t -> int
  val threshold_of_size : int -> t list -> t
  val within_threshold : Q.t -> t -> bool
  val pp_printer : Format.formatter -> t -> unit

  module Default : sig
    val ideal_density : Q.t
    val t_delta : Q.t
    val p_delta : Q.t
    val range_list : max_size:int -> t list
  end
end

module Packed_memory_array : sig
  type cfg = { density_scale : Density.t list; }

  type 'a t = {
    cells : 'a Kcas.Loc.t array;
    active_idx_range : int * int;
    config : cfg;
  }

  val make : int -> int t
end