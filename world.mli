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
(** Hunt the Wumpus world.

    This file describes the interface into the wumpus world.  Here we,
    basically define sensors and actuators ([move] or [shoot]) for the
    wumpus world.

    @author Ethan Burns *)

type t = {
  read : in_channel;
  write : out_channel;
  cave : cave;
}
    (** A wumpus world *)

and cave = {
  rooms : int;
  tunnels : int;
  bats : int;
  pits : int;
  max_arrows : int;
}
    (** A wumpus cave *)

and state = {
  num : int;
  adjacent : int list;
  arrows : int;
  draft : bool;
  stench : bool;
  rustle : bool;

  (* it is possible to enter a room with a pit and live. *)
  pit : bool;

}
    (** A state in the wumpus world. *)


exception Unexpected_input
  (** The wumpus program gave us unexpected input. *)


exception Invalid_move
  (** Invalid move attempt. *)


exception Dead
  (** We died. *)


exception Win
  (** We won. *)


val make_move : t -> (state -> int -> state)
  (** [make_move world] Makes a function that makes a move in the
      [world]. *)


val make_shoot : t -> (state -> int list -> state)
  (** [make_shoot world] Makes a function that shoots a magic arrow in
      the [world]. *)


val destroy : t -> unit
  (** [destroy w] Destroys the wumpus world. *)


val create : unit -> t * state
  (** [create]
      Create a new wumpus world.  The result is the tuple of the world and
      the initial state. *)
