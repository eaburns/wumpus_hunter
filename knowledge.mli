(*
 * Copyright (C) 2010 Ethan Burns
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
(** Knowledge about the wumpus world.

    @author Ethan A. Burns *)

module IntSet :
sig
  type elt = int
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val split : elt -> t -> t * bool * t
end


type t = {
  pit : state array;
  draft : state array;
  wumpus : state array;
  stench : state array;
  w_adj : state array;
  graph : IntSet.t array;
  cave : World.cave;
}


and state = Yes | No | Maybe


val dump_kb : t -> unit
  (** Dump the knowledge base to stdout. *)

val initial_kb : World.t -> World.state -> t
  (** Get the initial knowledge base from the given world and room.

      NOTE: Do not call the learn function on this initial KB with
      the initial room.  This will cause issues if the player starts
      in a room with a pit.  If the initial room is `learned' then we
      will discount this room from the possible rooms with pits
      (which isn't correct) *)


val make_learn : t -> bool -> World.state -> unit
  (** Builds a function that teaches the knowledge base information
      about a newly entered room!

      NOTE: The initial room is *not* a newly entered room unless you
      wander into it after having left it. *)


val make_wumpus_move : t -> unit -> unit
  (** -- Untested --

      Invalidate/update knowledge base to accomidate for the wumpus
      having moved to one of the rooms adjacent to it. *)


val make_reachable : t -> IntSet.elt -> (IntSet.elt * IntSet.elt list) list
  (** Build a function that will get a list of rooms that are
      reachable from the room with the given [rnum] with out
      wandering through a pit or a room with the wumpus. *)


val make_query : t -> int -> bool * state * state * state * state * state
  (** Build a function that queries the knowledge base for all of the
      facts that we know about the given room.

      The result is (explored, pit, draft, wumpus, stench, w_adj) *)


val make_get_wumpus_rooms : t -> int -> (int * int list) list
  (** Build a function that returns a list of rooms that the wumpus
      may be in and the path to a room adjacent to them. *)


val make_checkmate : t -> int -> (int * int list) option
  (** Build a function that tests for a checkmate condition (we see
      the wumpus). *)


val make_death_probability : t -> int -> float
  (** Get the probability that moving into the given room will cause
      death (either by wumpus or pit). *)
