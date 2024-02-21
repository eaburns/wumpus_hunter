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
(** A priority queue

    @author Ethan Burns *)

type 'a t = {
  mutable size: int;
  mutable fill: int;
  mutable data: (int * 'a) option array;
};;
(** The type of the priority queue. *)

exception Empty;;
(** Raised when an operation (take) is applied to an empty queue. *)

val create : int -> 'a t;;
(** Create a new priority queue. *)

val is_empty : 'a t -> bool;;
(** Check if the given queue is empty. *)

val add : int -> 'a -> 'a t -> unit;;
(** Add a value to the priority queue with the given priority *)

val take : 'a t -> int * 'a;;
(** Take the front value from the priority queue (with the lowest priority). *)

val iter : (int * 'a -> unit ) -> 'a t -> unit;;
(** Do function [f] for each element in the queue. *)
