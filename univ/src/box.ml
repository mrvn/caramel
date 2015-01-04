(* Universal container for GADTs
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

module type Boxed = sig
  type 'a value
  type t
  val box : 'a value -> t
  (* val unbox : t -> 'a value *)
  val to_string : t -> string
end

module MAKE(G : Eq.GADT) : Boxed with type 'a value = 'a G.t = struct
  type 'a value = 'a G.t
  type t = Box : 'a value -> t
    (*
  module Assoc = Eq.MAKE(struct type 'a t = 'a key end)
  let equal = Assoc.equal
  let cast_key = Assoc.cast
    *)
  let box v = Box v
  (* let unbox (Box v) = v *)

  (* Conversion to string needs universal functions, which requires
   * wrapping them in a record. Otherwise ocaml complains that the type
   * escapes.
   *)
  type conv = {
    value : 'a . 'a value -> string;
  }
  let conv = {
    value = G.to_string
  }
  let to_string (Box v) = conv.value v
end
