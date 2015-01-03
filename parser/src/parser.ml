(* GADT enabled recursive parser generator
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

module type Grammar = sig
  type 'a n
  type 'a token
  type 'a attrib
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option
  type lexeme
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val string_of_lexemes : lexeme Input.t -> string
  type 'a symbol =
  | NT of 'a n
  | T of 'a token
  val string_of_symbol : 'a symbol -> string
  type ('a, 'b) symbols =
  | Ret : ('a, 'a) symbols
  | Arg : 'a symbol * ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  val string_of_symbols : ('a, 'b) symbols -> string
  val ret : ('a, 'a) symbols
  val ( ^^^ ) : 'a symbol -> ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  type ('a, 'b) production = P of 'a n * ('a, 'b) symbols
  val string_of_production : ('a, 'b) production -> string
  type 'a attribution = A of 'a
  type ('a, 'b) rule = Rule : ('a, 'b) production * 'b attribution -> ('a, 'b) rule
  val string_of_rule : ('a, 'b) rule -> string
  type rules =
  | Nil : rules
  | Cons : ('a, 'b) rule * rules -> rules
  val string_of_rules : string -> rules -> string
  val ( @@@ ) : ('a, 'b) rule -> rules -> rules
  type start_nt
  type start_attrib
  val start : (start_nt, start_attrib) rule
  val rules : rules
end

module MAKE(G : Grammar) = struct
  open G
  exception ParseError
  exception ParseMatch

  let rec parse : type a b . int -> (a, b) G.rule -> 'c -> (a * 'c) = fun depth rule lexemes ->
    let indent = String.make depth ' '
    in
    Printf.printf "%sparse '%s' << '%s'\n%!" indent (G.string_of_rule rule) (string_of_lexemes lexemes);
    match rule with
    | Rule (P (nt, Ret), A attribution) ->
      Printf.printf "%sGot NT %s\n%!" indent (string_of_n nt); (attribution, lexemes)
    | Rule (P (nt, Arg (NT sym, rest)), A attribution) ->
      Printf.printf "%sRecursing into NT %s\n%!" indent (string_of_n sym);
      let rec loop = function
        | G.Nil -> raise ParseError
        | G.Cons (rule, rules) ->
          let cast : type a b c . (a, b) Eq.t -> (a, c) rule -> (b, c) rule = fun Eq.Refl x -> x in
          let rule_opt : type a b c . a n -> (b, c) rule -> (a, c) rule option = fun sym rule ->
            let Rule (P (new_nt, _), _) = rule in
            match G.n_equal new_nt sym with
            | None -> None
            | Some equality ->  Some (cast equality rule)
          in
          try
            match rule_opt sym rule with
            | None -> raise ParseError
            | Some (new_rule) ->
              Printf.printf "%sTrying rule %s\n%!" indent (string_of_rule new_rule);
              let (attrib, lexemes) = parse (depth + 1) new_rule lexemes
              in
              Printf.printf "%scontinuing\n%!" indent;
              parse depth (Rule (P (nt, rest), A (attribution attrib))) lexemes
          with ParseError -> loop rules
      in
      loop rules
    | Rule (P (nt, Arg (T token, rest)), A attribution) ->
      Printf.printf "%sMatching token %s\n%!" indent (string_of_token token);
      match lexemes with
      | [] -> Printf.printf "%sout of lexemes\n%!" indent; raise ParseError
      | (pos, lexeme) :: lexemes ->
        let value =
          match attrib_opt token lexeme with
          | Some attrib -> value_of_attrib attrib
          | None -> Printf.printf "%sfailed\n%!" indent; raise ParseError
        in
        parse (depth + 1) (Rule (P(nt, rest), A (attribution value))) lexemes
end
