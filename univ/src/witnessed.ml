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
  type t = Box : 'a key * 'a value -> t
  (* Conversion needs universal functions, which requires wrapping
   * them in a record. Otherwise ocaml complains that the type escapes.
   *)
  type ('a, 'b) conv = {
    key : 'c . 'c key -> 'a;
    value : 'c . 'c value -> 'b;
  }
  val box : 'a key -> 'a value -> t
  val unbox_opt : 'a key -> t -> ('a key * 'a value) option
  val value_opt : 'a key -> t -> 'a value option
  val as_strings : t -> (string * string)
  val conv : ('a, 'b) conv -> t -> ('a * 'b)
  val has_key : 'a key -> t -> bool
  val lookup : 'a key -> t list -> 'a value list
end

module MAKE(W : Witness) : Witnessed with type 'a key = 'a W.key
				     and type 'a value = 'a W.value = struct
  include W

  type t = Box : 'a key * 'a value -> t
  module G = struct
    type 'a t = 'a key
    let to_string = W.string_of_key
  end
  module Assoc = Eq.MAKE(G)
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

  (* Conversion needs universal functions, which requires wrapping
   * them in a record. Otherwise ocaml complains that the type escapes.
   *)
  type ('a, 'b) conv = {
    key : 'c . 'c key -> 'a;
    value : 'c . 'c value -> 'b;
  }
  let conv conv (Box (k, v)) = (conv.key k, conv.value v)
    
  let string_conv = {
    key = string_of_key;
    value = string_of_value;
  }
  let as_strings t = conv string_conv t

  let has_key key (Box (k, v)) =
    match equal k key with
    | None -> false
    | Some _ -> true

  let lookup : type a . a key -> t list -> a value list = fun key list ->
    let rec loop acc = function
      | [] -> acc
      | x :: xs ->
        loop
          (match value_opt key x with
          | None -> acc
          | Some v -> v :: acc)
          xs
    in
    loop [] list
end
