(* Grammar types generator for building grammars
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

module type Symbols = sig
  type 'a n
  type 'a token
  type 'a attrib
  type lexeme
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val string_of_lexemes : lexeme list -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option
end

module type Types = sig
  type 'a n
  type 'a token
  type 'a attrib
  type lexeme
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val string_of_lexemes : lexeme list -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option
  type 'a symbol = NT of 'a n | T of 'a token
  val string_of_symbol : 'a symbol -> string
  type (_, _) symbols =
  | Ret : ('a, 'a) symbols
  | Arg : 'a symbol * ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  val string_of_symbols : ('a, 'b) symbols -> string
  val ret : ('a, 'a) symbols
  val ( ^^^ ) : 'a symbol -> ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  type ('a, 'b) production = P of 'a n * ('a, 'b) symbols
  val string_of_production : ('a, 'b) production -> string
  type 'a attribution = A of 'a
  type ('a, 'b) rule =
  | Rule : ('a, 'b) production * 'b attribution -> ('a, 'b) rule
  val string_of_rule : ('a, 'b) rule -> string
  type rules =
  | Nil : rules
  | Cons : ('a, 'b) rule * rules -> rules
  val string_of_rules : string -> rules -> string
  val ( @@@ ) : ('a, 'b) rule -> rules -> rules
end

module MAKE(S : Symbols) : Types with type 'a n = 'a S.n
				 and type 'a token = 'a S.token
				 and type lexeme = S.lexeme = struct
  type 'a n = 'a S.n
  type 'a token = 'a S.token
  type 'a attrib = 'a S.attrib
  type lexeme = S.lexeme
  let string_of_n = S.string_of_n
  let string_of_token = S.string_of_token
  let string_of_attrib = S.string_of_attrib
  let string_of_lexemes = S.string_of_lexemes
  let attrib_opt = S.attrib_opt
  let value_of_attrib = S.value_of_attrib
  let n_equal n1 n2 = S.n_equal n1 n2

  type 'a symbol =
  | NT of 'a n
  | T of 'a token

  let string_of_symbol = function
  | NT n -> string_of_n n
  | T token -> string_of_token token

  type (_, _) symbols =
  | Ret : ('a, 'a) symbols
  | Arg : 'a symbol * ('b, 'c) symbols -> ('b, 'a -> 'c) symbols

  let rec string_of_symbols symbols =
    let rec loop : type a b . string -> string -> (a, b) symbols -> string
    = fun sep str symbols ->
      match symbols with
      | Ret -> str
      | Arg (symbol, symbols) ->
        loop " " (str ^ sep ^ string_of_symbol symbol) symbols
    in
    loop "" "" symbols

  let ret : ('a, 'a) symbols = Ret
  let ( ^^^ ) x y = Arg (x, y)

  type ('a, 'b) production = P of 'a n * ('a, 'b) symbols

  let string_of_production = function
    | P (n, symbols) ->
      Printf.sprintf "%s := %s" (string_of_n n) (string_of_symbols symbols)

  type 'a attribution = A of 'a

  type ('a, 'b) rule =
  | Rule : ('a, 'b) production * 'b attribution -> ('a, 'b) rule

  let string_of_rule (Rule (production, _)) = string_of_production production

  type rules =
  | Nil : rules
  | Cons : ('a, 'b) rule * rules -> rules

  let string_of_rules prefix rules =
    let sep2 = "\n" ^ prefix in
    let rec loop sep str = function
      | Nil -> str
      | Cons (rule, rules) ->
        loop sep2 (str ^ sep ^ (string_of_rule rule)) rules
    in
    loop prefix "" rules

  let ( @@@ ) rule rules = Cons (rule, rules)
end
