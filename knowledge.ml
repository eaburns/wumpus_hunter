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

module IntSet =
  Set.Make(struct
	     type t = int
	     let compare = compare
	   end)


type t = {
  pit : state array;
  draft : state array;
  wumpus: state array;
  stench : state array;
  w_adj: state array;
  graph : IntSet.t array;
  cave : World.cave;
}


and state = Yes | No | Maybe


let intset_of_list lst =
  (** Build an IntSet from a list of ints. *)
  List.fold_left
    (fun x set -> IntSet.add set x)
    IntSet.empty
    lst


let state_of_bool b =
  (** Get the state representation of a boolean. *)
  if b then Yes else No


let string_of_state s =
  (** Get the string representation of the given state. *)
  match s with
    | Yes -> "Yes"
    | No -> "No"
    | Maybe -> "Maybe"


let string_of_array kb ary =
  (** Get the name of the array [ary] if it is one of [kb]'s arrays *)
  if kb.pit == ary then
    "Pit"
  else if kb.draft == ary then
    "Draft"
  else if kb.wumpus == ary then
    "Wumpus"
  else if kb.stench == ary then
    "Stench"
  else if kb.w_adj == ary then
    "W_Adj"
  else "<unknown>"


let negate s =
  (** Get the negation of the given state value *)
  if s = Yes then No else Yes


let make_explored kb =
  (** Build an explored predicate *)
  (fun rnum ->
     (IntSet.cardinal kb.graph.(pred rnum)) = kb.cave.World.tunnels)


let away_from kb rnum radius =
  (** Get the rooms that the given [rnum] is [radius] rooms away from.
      This function starts by looking from the outside rooms in.  In
      other words, we find rooms that have [rnum] as an adjacency, then
      look for all the rooms that have those rooms as an adjacency,
      etc. *)
  let adj_to rnum =
    let rec add_adj_to ?(accum = IntSet.empty) n =
      if n == 0 then accum
      else
	if (make_explored kb n) && (IntSet.mem rnum (kb.graph.(pred n))) then
	  add_adj_to ~accum:(IntSet.add n accum) (pred n)
	else
	  add_adj_to ~accum:accum (pred n) in
      add_adj_to kb.cave.World.rooms in
  let rec add_away_from ?(accum = IntSet.empty) set r =
    if r = 0 then IntSet.union accum set
    else
      let next_layer =
	IntSet.fold
	  (fun x set -> IntSet.union (adj_to x) set)
	  set
	  IntSet.empty in
	add_away_from ~accum:(IntSet.union accum set) next_layer (pred r) in
    add_away_from (adj_to rnum) (pred radius)


let radius_size kb radius =
  (** The number of rooms that are within a given [radius] away *)
  let pow x y =
    if y = 0 then 1
    else let p = ref 0 in for i = 1 to y do p := !p * x done; !p in
  let rec rad_size r =
    if r = 0 then 0
    else (pow kb.cave.World.tunnels r) + (rad_size (pred r)) in
    rad_size radius


let make_covers kb ary_c c_val ary_a a_val rad num =
  (** Check if all rooms in [ary_a] that have value [a_val] are covered
      by a room in [ary_c] that has [c_val] and is within radius [rad],
      given that there are [num] rooms that can have [c_val] in
      [ary_c], and given that [rnum] is assigned [value]. *)
  let around kb rnum radius =
    (* Get the rooms that are [radius] rooms around [rnum].
       This function differs from away_from because it starts from the
       given room number and looks outward. *)
    let rec add_around ?(accum = IntSet.empty) set r =
      if r = 0 then IntSet.union accum set
      else
	let next_layer =
	  IntSet.fold
	    (fun x set -> IntSet.union kb.graph.(pred x) set)
	    set
	    IntSet.empty in
	  add_around ~accum:(IntSet.union accum set) next_layer (pred r) in
      add_around kb.graph.(pred rnum) (pred radius) in
  let build_graph rnum value =
    (* build the constraint graph with [rnum] assigned [value] in the
       [ary_c] array *)
    let lst = ref []
    and left = ref (if value = c_val then pred num else num) in
      for i = 1 to kb.cave.World.rooms do
	if ary_a.(pred i) = a_val then begin
	  let around =
	    IntSet.filter
	      (fun i -> ary_c.(pred i) <> negate c_val)
	      (if value <> c_val then
		 IntSet.filter (( <> ) rnum) (around kb i rad)
	       else (around kb i rad)) in
	    lst := (i, around) :: !lst
	end;
	if i <> rnum && ary_c.(pred i) = c_val then decr left
      done;
      (List.filter
	 (fun (_, s) ->
	    (not (IntSet.exists
		    (fun r -> ary_c.(pred r) = c_val)
		    s))
	    && (not (value = c_val && (IntSet.mem rnum s))))
	 !lst,
       !left) in
  let expand g =
    (* get all of the expansions of the graph [g] in which we assign
       a value [a_val] to a room in [ary_a] and eliminate nodes that
       are now covered. *)
    let arounds =
      List.fold_left
	(fun set x -> IntSet.union set x)
	IntSet.empty
	(snd (List.split g)) in
    let q = Queue.create () in
      IntSet.iter
	(fun x ->
	   let new_graph =
	     List.filter
	       (fun (_, set) -> not (IntSet.mem x set))
	       g in
	     Queue.add new_graph q)
	arounds;
      q in
  let rec validate g n =
    (* validate that we can distribute [n] [ary_a]=[a_val]'s
       over the given graph [g]. *)
    let o = Queue.create ()
    and c = Hashtbl.create 100
    and valid = ref false in
      Queue.add (n, g) o;
      while not (Queue.is_empty o) && not !valid do
	let left, g = Queue.take o in
	  if g = [] then valid := true
	  else if left > 0 then
	    Queue.iter
	      (fun g ->
		 if not (Hashtbl.mem c g) then begin
		   Queue.add (pred left, g) o;
		   Hashtbl.add c g true;
		 end)
	      (expand g)
      done;
      !valid in
    (fun rnum value ->
       (* Test if assigning [value] to the [rnum] room for the
	  [ary_c] array will still allow an allocation of the
	  remaining [num] [ary_c]=[c_val]'s to the cover the values
	  in [ary_a] with the value [a_val]. *)
       let g, n = build_graph rnum value in
	 n >= 0 && validate g n)


let dump_kb kb =
  (** Dump the knowledge base to stdout. *)
  let string_of_state = function
    | Yes -> "Yes"
    | No -> "No"
    | Maybe -> "Maybe" in
    Printf.printf
      "\n%3s %6s %6s %6s %6s %6s POSSIBLE TUNNELS\n"
      "NUM"
      "PIT"
      "DRAFT"
      "WUMPUS"
      "STENCH"
      "W_ADJ";
    for i = 0 to pred kb.cave.World.rooms do
      let rnum = succ i in
	Printf.printf
	  "%02d: %6s %6s %6s %6s %6s "
	  rnum
	  (string_of_state kb.pit.(i))
	  (string_of_state kb.draft.(i))
	  (string_of_state kb.wumpus.(i))
	  (string_of_state kb.stench.(i))
	  (string_of_state kb.w_adj.(i));
	IntSet.iter (fun x -> Printf.printf "%d " x) kb.graph.(i);
	print_newline ()
    done


let make_pit_constraint kb =
  (** Build a function that tests the pit constraints for a given room
      number with the given value. *)
  let covers_drafts =
    make_covers
      kb
      kb.pit Yes
      kb.draft Yes
      1
      kb.cave.World.pits in
    (fun rnum value ->
       (if value = Yes then
	  not (IntSet.exists
		 (fun x -> kb.draft.(pred x) = No)
		 (away_from kb rnum 1))
	else true)
       && covers_drafts rnum value)


let make_wumpus_constraint kb =
  (** Build a function that tests the wumpus constraints for a
      given room number with the given value. *)
  let covers_stenches =
    make_covers
      kb
      kb.wumpus Yes
      kb.stench Yes
      2
      1
  and covers_w_adjs =
    make_covers
      kb
      kb.wumpus Yes
      kb.w_adj Yes
      1
      1 in
    (fun rnum value ->
       (if value = Yes then
	  ((not (IntSet.exists
		   (fun x -> kb.stench.(pred x) = No)
		   (away_from kb rnum 2)))
	   && (not (IntSet.exists
		      (fun x -> kb.w_adj.(pred x) = No)
		      (away_from kb rnum 1))))
	else true)
       && covers_stenches rnum value
       && covers_w_adjs rnum value)


let make_w_adj_constraint kb =
  (** Build a function that tests the w_adj constraints for a given
      room number with the given value.  *)
  (fun rnum value ->
     if value = Yes then
       not (IntSet.exists
	      (fun x -> kb.stench.(pred x) = No)
	      (away_from kb rnum 1))
	 (* XXX -- also test for the case where we've seen the wumpus
	    and it is not adjacent. *)
     else true)


let make_constraints kb =
  (** Build a list of all of the constraints for this knowledge base. *)
  [ (kb.pit, make_pit_constraint kb);
    (kb.wumpus, make_wumpus_constraint kb);
    (kb.w_adj, make_w_adj_constraint kb); ];;


let make_propogate kb =
  (** Build function that propogates the constraints.  This,
      essentially does arc consistency. *)
  let update_domain test ary rnum =
    if ary.(pred rnum) = Maybe then begin
      if not (test rnum Yes) then begin
	ary.(pred rnum) <- No;
	true
      end else if not (test rnum No) then begin
	ary.(pred rnum) <- Yes;
	true
      end else false
    end else if not (test rnum ary.(pred rnum)) then begin
      dump_kb kb;
      Printf.printf
	"Invalid knowledge:\n%d: %s %s\n"
	rnum
	(string_of_array kb ary)
	(string_of_state ary.(pred rnum));
      assert false
    end else false in
  let constraints = make_constraints kb in
    (fun () ->
       let changed = ref true in
	 while !changed do
	   for i = 1 to kb.cave.World.rooms do
	     changed :=
	       List.fold_left
		 (fun ch (ary, test) ->
		    (update_domain test ary i) || ch)
		 false
		 constraints
	   done
	 done)


let initial_kb world room =
  (** Get the initial knowledge base from the given world and room.

      NOTE: Do not call the learn function on this initial KB with
      the initial room.  This will cause issues if the player starts
      in a room with a pit.  If the initial room is `learned' then we
      will discount this room from the possible rooms with pits
      (which isn't correct) *)
  let rec ints_from_one_to ?(accum = []) n =
    if n = 0 then accum
    else ints_from_one_to ~accum:(n :: accum) (pred n) in
  let rooms = world.World.cave.World.rooms
  and rnum = room.World.num in
  let pit = Array.make rooms Maybe
  and draft = Array.make rooms Maybe
  and wumpus = Array.make rooms Maybe
  and stench = Array.make rooms Maybe
  and w_adj = Array.make rooms Maybe
  and graph = Array.make rooms (intset_of_list (ints_from_one_to rooms)) in
    draft.(pred rnum) <- state_of_bool room.World.draft;
    stench.(pred rnum) <- state_of_bool room.World.stench;
    graph.(pred rnum) <- intset_of_list room.World.adjacent;
    let kb = { pit = pit;
	       draft = draft;
	       wumpus = wumpus;
	       stench = stench;
	       w_adj = w_adj;
	       graph = graph;
	       cave = world.World.cave } in
      make_propogate kb ();
      kb


let make_learn kb =
  (** Builds a function that teaches the knowledge base information
      about a newly entered room!

      NOTE: The initial room is *not* a newly entered room unless you
      wander into it after having left it. *)
  let propogate = make_propogate kb
  and explored = make_explored kb in
    (fun moved room ->
       let rnum = room.World.num in
	 if not (explored rnum) then begin
	   if moved then
	     kb.pit.(pred rnum) <- state_of_bool room.World.pit;
	   kb.wumpus.(pred rnum) <- No;
	   kb.draft.(pred rnum) <- state_of_bool room.World.draft;
	   kb.stench.(pred rnum) <- state_of_bool room.World.stench;
	   kb.graph.(pred rnum) <- intset_of_list room.World.adjacent;
	 end;
	 propogate ())


let make_wumpus_move kb =
  (** Invalidate/update knowledge base to accomidate for the wumpus
      having moved to one of the rooms adjacent to it. *)
  (fun () ->
     for i = 0 to pred kb.cave.World.rooms do
       kb.wumpus.(i) <- Maybe;
       kb.w_adj.(i) <- Maybe;
       kb.stench.(i) <- Maybe;
     done;
     dump_kb kb;)


let make_reachable kb =
  (** Build a function that will get a list of rooms that are
      reachable from the room with the given [rnum] with out
      wandering through a pit or a room with the wumpus. *)
  let explored = make_explored kb in
    (fun rnum ->
       let o = Prio_queue.create kb.cave.World.rooms
       and c = Hashtbl.create kb.cave.World.rooms in
	 Prio_queue.add 0 (rnum, []) o;
	 Hashtbl.add c rnum [];
	 while not (Prio_queue.is_empty o) do
	   let depth, (num, path) = Prio_queue.take o in
	     if explored num then begin
	       let children =
		 List.map
		   (fun child -> (child, child :: path))
		   (IntSet.elements
		      (IntSet.filter
			 (fun child ->
			    not (kb.pit.(pred child) = Yes)
			    && not (kb.wumpus.(pred child) = Yes)
			    && not (Hashtbl.mem c child))
			 kb.graph.(pred num))) in
		 List.iter
		   (fun (child, child_path) ->
		      Prio_queue.add (pred depth) (child, child_path) o;
		      Hashtbl.add c child child_path)
		   children
	     end
	 done;
	 List.sort
	   (fun (_, a) (_, b) -> compare (List.length a) (List.length b))
	   (Hashtbl.fold
	      (fun child path lst -> (child, List.rev path) :: lst)
	      c
	      []))


let make_query kb =
  (** Build a function that queries the knowledge base for all of the
      facts that we know about the given room.

      The result is (explored, pit, draft, wumpus, stench, w_adj) *)
  let explored = make_explored kb in
    (fun rnum ->
       (explored rnum,
	kb.pit.(pred rnum),
	kb.draft.(pred rnum),
	kb.wumpus.(pred rnum),
	kb.stench.(pred rnum),
	kb.w_adj.(pred rnum)))


let make_path_adjacent_to kb cur rnum =
  let adj = away_from kb rnum 1
  and reachable = make_reachable kb cur in
  let paths = (List.filter
		 (fun (i, _) -> IntSet.mem i adj)
		 reachable) in
    if paths = [] then begin None
    end else Some (rnum, (snd (List.hd paths)))


let make_get_wumpus_rooms kb =
  (** Get the list of rooms that may have a wumpus in them and the
      path to a room adjacen to it. *)
  let path_adjacent_to = make_path_adjacent_to kb in
    (fun rnum ->
       let _, maybes = (Array.fold_left
		       (fun (ind, lst) vl ->
			  if vl = Maybe then succ ind, succ ind :: lst
			  else succ ind, lst)
		       (0, []) kb.wumpus) in
	 List.map
	   (function
	      | Some p -> p
	      | None -> assert false)
	   (List.filter
	      (function
		 | Some (r, p) when r <> rnum -> true
		 | _ -> false)
	      (List.map (path_adjacent_to rnum) maybes)))


let make_checkmate kb =
  (** Build a function that tests for a checkmate condition (we see
      the wumpus). *)
  let away_from = away_from kb
  and path_adjacent_to = make_path_adjacent_to kb in
    (fun rnum ->
       let wumpus = ref None in
	 for i = 1 to kb.cave.World.rooms do
	   if kb.wumpus.(pred i) = Yes then
	     wumpus := Some i
	 done;
	 match !wumpus with
	   | None -> Printf.printf "No wumpus\n"; None
	   | Some i when IntSet.mem rnum (away_from i 1) -> Some (i, [])
	   | Some i -> path_adjacent_to rnum i)


let make_get_probability ary value total =
  (** Get the probability that the given room has a pit given that we
      know how many pits there are and how many rooms may contain pits. *)
  (fun rnum ->
     if ary.(pred rnum) = value then 1.0
     else if ary.(pred rnum) = (negate value) then 0.0
     else
       let count, maybes =
	 Array.fold_left
	   (fun (count, maybes) x ->
	      if x = value then (succ count, maybes)
	      else if x = Maybe then (count, succ maybes)
	      else (count, maybes))
	   (0, 0)
	   ary in
       let remaining = total - count in
	 (float_of_int remaining) /. (float_of_int maybes))


let make_death_probability kb =
  (** Get the probability that moving into the given room will cause
      death (either by wumpus or pit). *)
  let pit_prob = make_get_probability kb.pit Yes kb.cave.World.pits
  and wumpus_prob = make_get_probability kb.wumpus Yes 1 in
    (fun rnum ->
       let p_pit = pit_prob rnum
       and w_pit = wumpus_prob rnum in
	 (p_pit +. w_pit -. (p_pit *. w_pit)))

