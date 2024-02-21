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
(** A simple wump harness that logs statistics about a person's
    playing.  This program is used to collect data from a human
    player.

    @author Ethan A. Burns *)

let main =
  let move_count = ref 0
  and start_time = Unix.gettimeofday () in
  let finished s =
    let delta = (Unix.gettimeofday ()) -. start_time in
      incr move_count;
      (Printf.printf
	 "You %s!  You made %d moves in %.2f seconds\n"
	 s !move_count delta);
      (Printf.printf "Thanks for playing\n");
      exit 0 in
  let rec next_move move shoot current =
    (* get the next move from the human player. *)
    flush stdout;
    let c = Scanf.scanf " %c" (fun c -> c) in
      match c with
	| 'm' ->
	    let r = Scanf.scanf " %d" (fun d -> d) in
	      begin
		try
		  move current r
		with World.Invalid_move ->
		  Printf.printf "*Oof!* (You hit the wall)\n";
		  next_move move shoot current
	      end
	| 's' ->
	    let rec rooms ?(accum = []) b =
	      try
		let r = Scanf.bscanf b " %d" (fun d -> d) in
		  rooms ~accum:(r :: accum) b
	      with End_of_file -> List.rev accum in
	    let b = Scanf.Scanning.from_string (input_line stdin) in
	    shoot current (rooms b);
	| 'q' ->
	    finished "quit";
	| x ->
	    Printf.printf "I don't understand!\n";
	    next_move move shoot current in
    (fun () ->
       let world, room = World.create () in
       let move = World.make_move world
       and shoot = World.make_shoot world in
       let kb = Knowledge.initial_kb world room in
       let learn = Knowledge.make_learn kb in
       let room = ref room in
	 try
	   while true do
	     let shoot_fun cur rooms =
	       let r = shoot cur rooms in
		 Knowledge.make_wumpus_move kb ();
		 r in
	     let r = next_move move shoot_fun !room in
	       learn (r <> !room) !room;
	       room := r;
	       incr move_count;
	       Knowledge.dump_kb kb;
	   done
	 with
	   | World.Dead ->
	       World.destroy world;
	       finished "died";
	   | World.Win ->
	       World.destroy world;
	       finished "won")
let _ = main ()
