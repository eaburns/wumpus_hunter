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
(** The main program for the wumpus hunter AI.

    @author Ethan Burns. *)

let main =
  let move_count = ref 0
  and start_time = Unix.gettimeofday ()
  and random = ref false in

  let finished s =
    (** Print the finished message. *)
    let delta = (Unix.gettimeofday ()) -. start_time in
      incr move_count;
      (Printf.printf
	 "I %s!  I made %d moves in %.2f seconds\n"
	 s !move_count delta);
      (Printf.printf "Thanks for playing");
      exit 0 in

    (fun () ->
       Arg.parse ["-random", Arg.Set random, "random exploration";]
	 (fun _ -> ()) "Usage: hunter [options...]";
       let world, room = World.create () in
       let kb = Knowledge.initial_kb world room in
       let learn = Knowledge.make_learn kb in
       let next_move = Plan.make_next_move !random world kb in
       let room = ref room in
	 try
	   while true do
	     let r, moves = (next_move !room) in
	       learn (r <> !room) r;
	       room := r;
	       move_count := !move_count + moves;
	       Knowledge.dump_kb kb;
	   done
	 with
	   | World.Dead ->
	       World.destroy world;
	       finished "died"
	   | World.Win ->
	       World.destroy world;
	       finished "won")

let _ = main ()
