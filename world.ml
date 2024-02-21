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

    @author Ethan Burns *)

type t = {
  read : in_channel;
  write : out_channel;
  cave : cave;
}

and cave = {
  rooms : int;
  tunnels : int;
  bats : int;
  pits : int;
  max_arrows : int;
}

and state = {
  num : int;
  adjacent : int list;
  arrows : int;
  draft : bool;
  stench : bool;
  rustle : bool;

  (* It is possible to enter a room with a pit and live.
     NOTE: this is only valid if it is the first time entering a room. *)
  pit : bool;
}

exception Unexpected_input
exception Invalid_move
exception Dead
exception Win


(** The instruction prompt string.  (just used for double checking) *)
let instruction_prompt = "Instructions? (y-n) "


(** Prompt strings. *)
let prompts = ["(y-n) ";
	       "(m-s) ";
	       (* "Good luck." delimits the cave persistant info *)
	       "Good luck.";
	      ]


(** [read_chan channel]
    Reads from the channel untill prompted.  Echos what was read. *)
let rec read_chan ?(buf = "") chan =
  let got_prompt =
    List.exists (fun p ->
		   let p_len = String.length p
		   and b_len = String.length buf in
		     if b_len < p_len then false
		     else
		       let tl = String.sub buf (b_len-p_len) p_len in
			 if tl = p then true
			 else false ) prompts in
    if got_prompt then begin print_string buf; buf end
    else read_chan ~buf:(buf ^ (String.make 1 (input_char chan))) chan


(** [read world]
    Reads from the world untill prompted.  Echos what was read. *)
let read w =  read_chan w.read


(** [write_chan channel string]
    Writes a string to the channel and echos what was written. *)
let write_chan chan str =
  print_string str;
  output_string chan str;
  flush chan


(** [write world string]
    Writes a string to the world and echos what was written. *)
let write w = write_chan w.write


(** [find_marker marker str]
    Looks for the string [marker] in the string [str].  The result is
    the remainder of [str] after [marker]. *)
let rec find_marker marker str =
  let m_len = String.length marker
  and s_len = String.length str in
    if s_len < m_len then raise Not_found
    else
      if (String.sub str 0 m_len) = marker then
	(String.sub str m_len (s_len - m_len))
      else
	find_marker marker (String.sub str 1 (pred s_len))


(** [contains marker str]
    Check if the string contains the marker. *)
let contains marker str =
  try begin ignore (find_marker marker str); true end
  with Not_found -> false


(** [sence world]
    Sence the current room in the given world.  The result is None if we
    died, or a room containing the current room's state.  You can only
    sence once per move since the game only ouputs after a move. *)
let sence w =
  let room str =
    let marker = "You are in room" in
      try
	Scanf.sscanf (find_marker marker str) " %d" (fun n -> n)
      with Not_found -> raise Unexpected_input in
  let adjacent str =
    let marker = "There are tunnels to rooms" in
      try
	Scanf.sscanf (find_marker marker str) " %d, %d, and %d"
	  (fun a b c -> [a; b; c])
      with Not_found -> raise Unexpected_input in
  let arrows str =
    let marker = "and have" in
      try
	Scanf.sscanf (find_marker marker str) " %d" (fun n -> n)
      with Not_found -> raise Unexpected_input in
  let rustle str =
    let marker = "*rustle*" in contains marker str in
  let draft str =
    let marker = "*whoosh*" in contains marker str in
  let stench str =
    let marker = "*sniff*" in contains marker str in
  let death str =
    let pit_death =
      let marker = "*AAAUUUUGGGGGHHHHHhhhhhhhhhh...*" in contains marker str in
    let wumpus_death =
      let marker = "*ROAR*" in contains marker str in
      pit_death || wumpus_death in
  let win str =
    let marker = "*thwock!* *groan* *crash*" in contains marker str in
  let pit str =
    let marker = "Without conscious thought" in contains marker str in
  let str = read w in
    if death str then raise Dead
    else if win str then raise Win
    else { num = room str;
	   adjacent = adjacent str;
	   arrows = arrows str;
	   draft = draft str;
	   stench = stench str;
	   rustle = rustle str;
	   pit = pit str; }


let make_move w =
  (fun state dest ->
     if not (List.exists (( = ) dest) state.adjacent) then raise Invalid_move
     else begin
       write w ("m " ^ (string_of_int dest) ^ "\n");
       sence w
     end)


let make_shoot w =
  (fun state path ->
     if state.arrows < 1 then raise Invalid_move
     else begin
       write w ("s" ^ (List.fold_left (fun s r -> s ^ " " ^ (string_of_int r))
			 "" path) ^ "\n");
       sence w
     end)


let destroy w =
  print_endline "Closing wump";
  ignore (Unix.close_process (w.read, w.write))


let create () =
  let read_cave chan =
    let str = read_chan chan in
    let number_after marker =
      try Scanf.sscanf (find_marker marker str) " %d" (fun n -> n)
      with Not_found -> raise Unexpected_input in
    let rooms =
      let marker = "cave with" in number_after marker
    and tunnels =
      let marker = "rooms and" in number_after marker
    and bats =
      let marker = "There are" in number_after marker
    and pits =
      let marker = "bats and" in number_after marker
    and max_arrows =
      let marker = "quiver holds" in number_after marker in
      { rooms = rooms;
	tunnels = tunnels;
	bats = bats;
	pits = pits;
	max_arrows = max_arrows } in
  let init_world read write =
    if (read_chan read) <> instruction_prompt then raise Unexpected_input
    else begin
      write_chan write "n\n";
      let cave = read_cave read in
	{ read = read; write = write; cave = cave }
    end in
(*
  let prog = "~/src/wump/a.out -t 3 -b 0 -p 3 -r 20" in
*)
  let prog = "wump -t 3 -b 0 -p 3 -r 20" in
  let read, write = Unix.open_process prog in
  let world = init_world read write in
    (world, sence world)
