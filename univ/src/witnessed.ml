(* Universal container for pairs of witness key and value
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

module type Witness = sig
  type 'a key
  type 'a value
  val string_of_key : 'a key -> string
  val string_of_value : 'a value -> string
end

module type Witnessed = sig
  type 'a key
  type 'a value
  type t
  val box : 'a key -> 'a value -> t
  val unbox_opt : 'a key -> t -> ('a key * 'a value) option
  val value_opt : 'a key -> t -> 'a value option
  val as_strings : t -> (string * string)
end

module MAKE(W : Witness) : Witnessed with type 'a key = 'a W.key
				     and type 'a value = 'a W.value = struct
  type 'a key = 'a W.key
  type 'a value = 'a W.value
  type t = Box : 'a key * 'a value -> t
  module Assoc = Eq.MAKE(struct type 'a t = 'a key end)
  let equal = Assoc.equal
  let cast_key = Assoc.cast
  let cast_value : type a b . (a, b) Eq.t -> a value -> b value
		     = fun Eq.Refl x -> x
  let box k v = Box (k, v)
  let unbox_opt : type a . a key -> t -> (a key * a value) option
		    = fun key box ->
    let Box (k, v) = box
    in
    match equal k key with
    | None -> None
    | Some equality -> Some ((cast_key equality k), (cast_value equality v))
  let value_opt : type a . a key -> t -> a value option = fun key box ->
    let Box (k, v) = box
    in
    match equal k key with
    | None -> None
    | Some equality -> Some (cast_value equality v)

  (* Conversion to string needs universal functions, which requires
   * wrapping them in a record. Otherwise ocaml complains that the type
   * escapes.
   *)
  type conv = {
    key : 'a . 'a key -> string;
    value : 'a . 'a value -> string;
  }
  let conv = {
    key = W.string_of_key;
    value = W.string_of_value;
  }
  let as_strings (Box (k, v)) = (conv.key k, conv.value v)
end;;
