(* GADT enabled Lexer generator
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

module type Rules = sig
  (* from LexerTypes *)
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

  (* new stuff *)
  type symbol
  type action =
    Pos.t * symbol list * symbol Input.t
    -> Pos.t * lexeme * symbol Input.t
  type rule = symbol Regexp.regexp * action
  type scan_one = symbol Input.t -> Pos.t * lexeme * symbol Input.t
  val specs : scan_one -> rule list
end

module MAKE(R : Rules) (* : (Lexer with ...) *) = struct
  include R

  module State = struct
    type t = R.rule list

    let filter pred l =
      let rec f l r =
        match l with
            [] -> List.rev r
          | x::xs ->
	    if pred x 
	    then f xs (x::r)
	    else f xs r
      in f l []

    let next state symbol =
      let after_state =
        List.map (function (regexp, action) ->
          Regexp.after_symbol symbol regexp, action)
          state
      in
      filter (function (regexp, _) ->
        not (Regexp.is_null regexp))
        after_state

    let initial_state scan_one = R.specs scan_one

    let matched_rules state =
      filter (function (regexp, _) ->
        Regexp.accepts_empty regexp)
        state

    let is_stuck state = state = []
  end

  (* Longest match *)
  exception Scan_error of Pos.t

  let rec scan_one stream =
    let start = Input.pos stream in
    let rec loop pos state rev_lexeme maybe_last_match stream =
      match stream with
      | [] -> maybe_last_match
      | _ when State.is_stuck state -> maybe_last_match
      | (new_pos, symbol) :: stream ->
	let new_pos = Pos.merge pos new_pos in
        let new_state = State.next state symbol in
        let new_matched = State.matched_rules new_state in
        let rev_lexeme = symbol::rev_lexeme in
        let maybe_last_match =
          match new_matched with
            | [] -> maybe_last_match
            | (_, action)::_ -> Some (action, rev_lexeme, pos, stream)
        in
        loop new_pos new_state rev_lexeme maybe_last_match stream
    in
    let (action, rev_lexeme, pos, stream) =
      match loop start (State.initial_state scan_one) [] None stream with
        | None -> raise @@ Scan_error start
        | Some last_match -> last_match
    in
    let (pos, lexeme, stream) = action (pos, List.rev rev_lexeme, stream)
    in
    (pos, lexeme, stream)
      
  let scan stream =
    let rec scan rev_result = function
      | [] -> List.rev rev_result
      | stream ->
        let (pos, lexeme, stream) = scan_one stream
        in
        scan ((pos, lexeme) :: rev_result) stream
    in
    scan [] stream
end
