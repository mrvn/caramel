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
  (* the type of the input symbols *)
  type symbol

  (* action to create a lexeme for a matched regexp, action can eat
     more input but then has to adjust pos accodringly *)
  type action =
    Pos.t * symbol list * symbol Input.t
    -> Pos.t * lexeme * symbol Input.t

  (* pair of a regexp and the action that should be called when it
     matches *)
  type rule = symbol Regexp.regexp * action

  (* type of the function scanning a single token *)
  type scan_one = symbol Input.t -> Pos.t * lexeme * symbol Input.t

  (* the rules for the lexer. scan_one is passed as first argument
     to allow rules that produce no lexeme. e.g. the rule to <space>
     can call scan_one again to skip over the <space> *)
  val specs : scan_one -> rule list
end

module MAKE(R : Rules) (* : (Lexer with ...) *) = struct
  include R

  module State = struct
    (* The state of the lexer is a list of rules where the begining
       was matched against the input. *)
    type t = R.rule list

    (* tail recursive List.find *)
    let filter pred l =
      let rec f l r =
        match l with
            [] -> List.rev r
          | x::xs ->
	    if pred x 
	    then f xs (x::r)
	    else f xs r
      in f l []

    (* generate the next state for a given symbol *)
    let next state symbol =
      (* advance all regexps by one symbol *)
      let after_state =
        List.map (function (regexp, action) ->
          Regexp.after_symbol symbol regexp, action)
          state
      in
      (* delete regexps that don't match (are null) *)
      filter (function (regexp, _) ->
        not (Regexp.is_null regexp))
        after_state

    (* initial state is just all the rules *)
    let initial_state scan_one = R.specs scan_one

    (* regexps that accept empty are complete matches for the input *)
    let matched_rules state =
      filter (function (regexp, _) ->
        Regexp.accepts_empty regexp)
        state

    (* if all regexps failed then the state is stuck *)
    let is_stuck state = state = []
  end

  (* scanning error with position for nice error reporting *)
  exception Scan_error of Pos.t

  (* scan one lexeme, find longest match and apply action *)
  let rec scan_one stream =
    (* start of the lexeme is the position of the first symbol *)
    let start = Input.pos stream in
    (* find longest match *)
    let rec loop pos state rev_lexeme maybe_last_match stream =
      match stream with
      | [] -> maybe_last_match (* end of stream, can't get any longer *)
	(* scanner is stuck, backtrack to last match *)
      | _ when State.is_stuck state -> maybe_last_match
	(* still scanning *)
      | (new_pos, symbol) :: stream ->
	(* include symbol in position *)
	let new_pos = Pos.merge pos new_pos in
	(* advance state by symbol *)
        let new_state = State.next state symbol in
	(* all rules that exactly match the input so far *)
        let new_matched = State.matched_rules new_state in
	(* add symbol to saved list of symbols so far *)
        let rev_lexeme = symbol::rev_lexeme in
	(* update maybe_last_match if there is at least one rule that
	   exactly matches the input so far *)
        let maybe_last_match =
          match new_matched with
            | [] -> maybe_last_match
            | (_, action)::_ -> Some (action, rev_lexeme, pos, stream)
        in
	(* try to find a longer match *)
        loop new_pos new_state rev_lexeme maybe_last_match stream
    in
    (* start the loop to find longest match *)
    let (action, rev_lexeme, pos, stream) =
      match loop start (State.initial_state scan_one) [] None stream with
      (* nothing did match, something must be wrong with the rules or input *)
      | None -> raise @@ Scan_error start
      (* return longest match *)
      | Some last_match -> last_match
    in
    (* apply action to generate (position, lexeme, remaning stream) *)
    action (pos, List.rev rev_lexeme, stream)

  (* scan the whole stream, convert symbol Input.t -> lexeme Input.t *)
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
