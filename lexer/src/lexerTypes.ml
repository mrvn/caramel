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
  (* The lexer splits the input into tokens. Each token has an
   * attribute giving extra data for the token. You usualy have a
   * token Indent with attribute being the name of the
   * identifier. Keywords usualy have a unit attribute.
   *
   * Example:
   * type _ token = Ident : string token | Plus : unit token
   * type _ attrib = Nil : unit attrib | String : string -> string * attrib
   * 
   * The tokens act as witness type for the attribute. The Ident token must
   * have a string attrib, which will be checked by the type system at at
   * compile time.
   *)
  type 'a token
  type 'a attrib
  (* for debug purposed *)
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  (* extract value from attribute, used later by the parser *)
  val value_of_attrib : 'a attrib -> 'a
end

(* Output of the types generator *)
module type LexerTypes = sig
  (* from TA *)
  type 'a token
  type 'a attrib
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val value_of_attrib : 'a attrib -> 'a

  type ('a, 'b) lexeme_conv = {
    key : 'c . 'c token -> 'a;
    value : 'c . 'c attrib -> 'b;
  }
  (* output type of the lexer (lexeme Input.t) *)
  type lexeme
  (* helper function to create a lexeme *)
  val lexeme : 'a token -> 'a attrib -> lexeme
  val lexeme_conv : ('a, 'b) lexeme_conv -> lexeme -> ('a * 'b)
  (* return Some attrib if the token matches the lexeme *)
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  (* return true if the token matches the lexeme *)
  val has_token : 'a token -> lexeme -> bool
  (* for debug purposes *)
  val string_of_lexeme : lexeme -> string
  val string_of_lexemes : lexeme Input.t -> string
end

(* create automatic types and helper functions *)
module MAKE(TA : TA) : LexerTypes with type 'a token = 'a TA.token
				  and type 'a attrib = 'a TA.attrib = struct
  include TA

  (* create universal type for lexemes *)
  module W = struct
    type 'a key = 'a token
    type 'a value = 'a attrib
    let string_of_key = string_of_token
    let string_of_value = string_of_attrib
  end
  module Boxed = Univ.Witnessed.MAKE(W)
  type lexeme = Boxed.t
  type ('a, 'b) lexeme_conv = ('a, 'b) Boxed.conv = {
    key : 'c . 'c Boxed.key -> 'a;     
    value : 'c . 'c Boxed.value -> 'b;
  }
    
  (* helper function to create a lexeme *)
  let lexeme token attrib = Boxed.box token attrib

  let lexeme_conv conv lexeme = Boxed.conv conv lexeme

  (* return Some attrib if the token matches the lexeme *)
  let attrib_opt token lexeme = Boxed.value_opt token lexeme

  (* return true if the token matches the lexeme *)
  let has_token token lexeme =
    match Boxed.value_opt token lexeme with
    | None -> false
    | _ -> true

  (* for debug purposes *)
  let string_of_lexeme lexeme =
    match Boxed.as_strings lexeme with
    | (k, "") -> k
    | (k, v) -> Printf.sprintf "(%s %s)" k v

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
