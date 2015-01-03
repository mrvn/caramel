(* Type equality of GADTs
 * Copyright (C) 2015 Goswin von Brederlow <goswin-v-b@web.de>
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

module Eq = struct
  type (_,_) t = Refl : ('a,'a) t
end
include Eq

module type GADT = sig
  type 'a t
end

module type Assoc = sig
  type 'a t
  (* equal t1 t2: return Some Refl if types match *)
  val equal : 'a t -> 'b t -> ('a, 'b) Eq.t option
  (* cast eq t: cast t from type a to b when type equality is witnessed *)
  val cast : ('a, 'b) Eq.t -> 'a t -> 'b t
  (* cast_opt t1 t2: return Some t1 cast to the type of t2 if types match *)
  val cast_opt : 'a t -> 'b t -> 'b t option
end

module MAKE(T : GADT) : Assoc with type 'a t = 'a T.t = struct
  type 'a t = 'a T.t
  (* equal t1 t2: return Some Refl if types match *)
  let equal : type a b . a t -> b t -> (a, b) Eq.t option = fun t1 t2 ->
    let t1 = Obj.repr t1 in
    let t2 = Obj.repr t2 in
    match (Obj.is_int t1, Obj.is_int t2) with
    | (true, true) -> (* both constructors without arguments *)
      if t1 = t2
      then Obj.magic (Some Eq.Refl)
      else None
    | (false, false) -> (* both constructors with arguments *)
      if Obj.tag t1 = Obj.tag t2
      then Obj.magic (Some Eq.Refl)
      else None
    | _ -> None (* mixed cases can't be equal *)
  (* cast eq t: cast t from type a to b when type equality is witnessed *)
  let cast : type a b . (a, b) Eq.t -> a t -> b t
		   = fun Eq.Refl x -> x
  (* cast_opt t1 t2: return Some t1 cast to the type of t2 if types match *)
  let cast_opt : type a b . a t -> b t -> b t option = fun t1 t2 ->
    match equal t1 t2 with
    | None -> None
    | Some equality -> Some (cast equality t1)
end
