(* $I1: Unison file synchronizer: src/lwt/pqueue.mli $ *)
(* $I2: Last modified by vouillon on Fri, 14 Sep 2001 12:35:32 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val add: elt -> t -> t
    val union: t -> t -> t
    val find_min: t -> elt
    val remove_min: t -> t
    val size: t -> int
  end

module Make(Ord: OrderedType) : S with type elt = Ord.t
