(* generate types and helper functions for the lexer generator
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

(* Token and attribute types as input for the types generator *)
module type TA = sig
  type 'a token
  type 'a attrib
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val value_of_attrib : 'a attrib -> 'a
end

(* Output of the types generator *)
module type LexerTypes = sig
  type 'a token
  type 'a attrib
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val value_of_attrib : 'a attrib -> 'a
  type lexeme
  val lexeme : 'a token -> 'a attrib -> lexeme
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val has_token : 'a token -> lexeme -> bool
  val string_of_lexeme : lexeme -> string
  val string_of_lexemes : lexeme Input.t -> string
end

module MAKE(TA : TA) : LexerTypes with type 'a token = 'a TA.token
				  and type 'a attrib = 'a TA.attrib = struct
  type 'a token = 'a TA.token
  type 'a attrib = 'a TA.attrib
  let string_of_token = TA.string_of_token
  let string_of_attrib = TA.string_of_attrib
  let value_of_attrib = TA.value_of_attrib
  module W = struct
    type 'a key = 'a token
    type 'a value = 'a attrib
    let string_of_key = string_of_token
    let string_of_value = string_of_attrib
  end
  module Boxed = Univ.Witnessed.MAKE(W)
  type lexeme = Boxed.t
  let lexeme token attrib = Boxed.box token attrib
  let attrib_opt token lexeme = Boxed.value_opt token lexeme
  let has_token token lexeme =
    match Boxed.value_opt token lexeme with
    | None -> false
    | _ -> true
  let string_of_lexeme lexeme =
    match Boxed.as_strings lexeme with
    | (k, "") -> k
    | (k, v) -> Printf.sprintf "(%s %s)" k v

  let string_of_input printer input =
    let rec loop sep str = function
      | [] -> str
      | (pos, symbol)::xs ->
	loop
	  " "
	  (Printf.sprintf "%s%s [%s]" sep (printer symbol) (Pos.string_of_t pos))
	  xs
    in
    loop "" "" input

  let string_of_lexemes lexemes =
    let (_, str) =
      List.fold_left
        (fun (sep, str) (pos, lexeme) ->
          (" ",
	   Printf.sprintf "%s%s%s [%s]"
	     str
	     sep
	     (string_of_lexeme lexeme)
	     (Pos.string_of_t pos)))
        ("", "")
        lexemes
    in
    str
end
