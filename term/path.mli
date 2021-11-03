type t = private
| Here
| Head of t
| Spine of int * t
| Body of t

type quotient = private
  { whole: t
  ; frac: (t * t) option
  }

(** [here] is [Here]. *)
val here: t

(** [head path] is [Head path]. *)
val head: t -> t

(** [spine i path] is [Spine (i, path)] or @raise Invalid_arg if [i] is
    negative. *)
val spine: int -> t -> t

(** [body path] is [Body path]. *)
val body: t -> t

(** [add lhs rhs] is [rhs] qualified by the prefix [lhs]. *)
val add: t -> t -> t

(** [div numer denom] is the quotient of [numer] divided by [denom]. More
    specifically, the common prefix of [numer] and [denom] is considered the
    "whole part" of the quotient, and if [numer] and [denom] have differing
    suffixes [numer_suffix, denom_suffix], this is considered the "fractional
    part". *)
val div: t -> t -> quotient