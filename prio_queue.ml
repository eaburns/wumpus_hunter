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
}

exception Empty

(** The minimum size of a priority queue. *)
let min_size = 10

let create init_size =
  let size = if init_size < min_size then min_size else init_size in
    { size = size ;
      fill = 0;
      data = Array.make size None; }

let is_empty q = q.fill = 0

(** Get the parent's index. *)
let parent_of i = (i - 1) / 2

(** Get the left child's index. *)
let left_of i = (2 * i) + 1

(** Get the right child's index. *)
let right_of i = (2 * i) + 2

(** Get the element at the given queue index. *)
let get_at_ind q i =
  if is_empty q then raise Empty
  else if q.fill < i then assert false
  else match q.data.(i) with
    | None -> assert false
    | Some node -> node

(** Set the element at the given index. *)
let set_at_ind q i e =
  try
    q.data.(i) <- Some e
  with Invalid_argument x ->
    (Printf.printf "invalid ind %d of %d\n" i (Array.length q.data));
    raise (Invalid_argument x)


(** Get the key of the given node. *)
let key_of = fst

(** Get the minimum valued child of the given index *)
let min_child q i =
  let left = left_of i and right = right_of i in
    if left < q.fill && right < q.fill then
      if (key_of (get_at_ind q left)) < (key_of (get_at_ind q right)) then
        left
      else
        right
    else if left < q.fill then left
    else if right < q.fill then right
    else assert false

let add k e q =
  let grow =
    if q.fill = q.size then begin
      let new_size = q.size * q.size in
      let new_ary = Array.make new_size None in
        Array.blit q.data 0 new_ary 0 q.fill;
        q.data <- new_ary;
        q.size <- new_size;
    end in
  let rec sift_up i =
    if i > 0 then
      let p = parent_of i in
      let pnode = get_at_ind q p and node = get_at_ind q i in
        if (key_of node) <= (key_of pnode) then begin
          set_at_ind q p node;
          set_at_ind q i pnode;
          sift_up p;
        end in

    grow;
    set_at_ind q q.fill (k, e);
    sift_up q.fill;
    q.fill <- succ q.fill

let take q =
  let is_leaf q i =
    let left = left_of i and right = right_of i in
      left >= q.fill && right >= q.fill in

  let rec sift_down i =
    if not (is_leaf q i) then begin
      let min_child_ind = min_child q i in
      let min_child = get_at_ind q min_child_ind
      and node = get_at_ind q i in
        if (key_of node) > (key_of min_child) then begin
          set_at_ind q min_child_ind node;
          set_at_ind q i min_child;
          sift_down min_child_ind
        end
    end in

    if q.fill = 0 then raise Empty
    else
      let ret = get_at_ind q 0
      and last = get_at_ind q (pred q.fill) in
        set_at_ind q 0 last;
        q.fill <- pred q.fill;
        sift_down 0;
        ret

let iter f q =
  for i = 0 to (pred q.fill) do
    f (get_at_ind q i)
  done
