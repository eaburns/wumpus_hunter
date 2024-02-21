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
(** The planner for the wumpus hunter

    @author Ethan A. Burns *)


let rec follow_path ?(count = 0) move cur path =
  (** Moves the given path from the current room. *)
  match path with
    | [] -> cur, count
    | hd :: tl ->
	follow_path ~count:(succ count) move (move cur hd) tl


let safe_rooms query reachable =
  (** Get a list of rooms that are completely safe to move to from a
      list of rooms *)
  List.filter
    (fun (_, path) ->
       List.for_all
	 (fun r -> let _, p, _, w, _, _ = query r in
	    p = Knowledge.No && w = Knowledge.No)
	 path)
    reachable


let unexplored_rooms query reachable =
  (** Get the list of unexplored rooms from the list of reachable rooms. *)
  List.filter
    (fun (rnum, _) -> let e, _, _, _, _, _ = query rnum in not e)
    reachable


let make_next_move rnd world kb =
  (** [next_move random world kb current]
      Build a function that performs the next available move *)
  let move = World.make_move world
  and shoot = World.make_shoot world
  and checkmate = Knowledge.make_checkmate kb
  and death_prob = Knowledge.make_death_probability kb
  and query = Knowledge.make_query kb
  and reachable = Knowledge.make_reachable kb
  and wumpus_move = Knowledge.make_wumpus_move kb
  and get_wumpus_rooms = Knowledge.make_get_wumpus_rooms kb
  and rooms_shot = ref [] in
  let try_shoot best_death_prob wumpus_rooms current =
    List.iter (fun (r, _) -> Printf.printf "%d " r) wumpus_rooms;
    Printf.printf "\n";
    let arrows =  current.World.arrows
    and length = (List.length wumpus_rooms)
    and unshot_room = (List.exists
			 (fun (r, _) -> not (List.mem r !rooms_shot))
			 wumpus_rooms) in
      Printf.printf "arrows: %d, length: %d, prob: %f, unshot_room: %s\n"
	arrows length best_death_prob
	(if unshot_room then "true" else "false");
      arrows > 1
      && length > 0
      && best_death_prob > 0.001
      && unshot_room in
  let explore unexplored current =
    if rnd then
      let r = (Random.int (List.length unexplored)) in
      let rnum, path = List.nth unexplored r in
	false, rnum, path
    else
      let wumpus_rooms = get_wumpus_rooms current.World.num
      and rnum, path =List.hd unexplored in
	if try_shoot (death_prob rnum) wumpus_rooms current then
	  let rnum, path = (List.find
			      (fun (r, _) ->
				 not (List.mem r !rooms_shot))
			      wumpus_rooms) in
	    rooms_shot := rnum :: !rooms_shot;
	    true, rnum, path
	else false, rnum, path in
  (fun current ->
     let reachable = reachable current.World.num in
     let unexplored =
       List.sort
	 (fun (a, _) (b, _) -> compare (death_prob a) (death_prob b))
	 (unexplored_rooms query reachable) in
       assert (reachable <> []);
       match checkmate current.World.num with
	 | Some (i, path) ->
	     Printf.printf "checkmate in room %d: " i;
	     List.iter (fun x -> Printf.printf "%d " x) path;
	     print_newline();
	     let adj_room, moves = follow_path move current path in
	     let cur, moves = shoot adj_room [i], succ moves in
	       wumpus_move (); cur, moves
	 | _ ->
	     if (unexplored = []) then failwith "No more rooms to look at";
	     let shoot_room, rnum, path = explore unexplored current in
	       if shoot_room then begin
		 Printf.printf "Shooting to %d: " rnum;
		 List.iter (Printf.printf "%d ") path;
		 print_newline ();
		 let adj_room, moves =
		   (follow_path move current path) in
		 let cur, moves = shoot adj_room [rnum], succ moves in
		   wumpus_move (); cur, moves
	       end else begin
		 Printf.printf "Moving to %d (prob death=%f)\n"
		   rnum (death_prob rnum);
		 follow_path move current path
	       end)

