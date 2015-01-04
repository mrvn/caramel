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
  val string_of_lexemes : lexeme Input.t -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option
end

module type Types = sig
  (* from Symbols *)
  type 'a n
  type 'a token
  type 'a attrib
  type lexeme
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val string_of_lexemes : lexeme Input.t -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option

  (* new *)
  type terminal
  val terminal : 'a token -> terminal
  val string_of_terminal : terminal -> string
  type nonterminal
  val nonterminal : 'a n -> nonterminal
  val string_of_nonterminal : nonterminal -> string
  type 'a symbol = NT of 'a n | T of 'a token
  val string_of_symbol : 'a symbol -> string
  type (_, _) symbols =
  | Ret : ('a, 'a) symbols
  | Arg : 'a symbol * ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  val string_of_symbols : ('a, 'b) symbols -> string
  val ret : ('a, 'a) symbols
  val ( ^^^ ) : 'a symbol -> ('b, 'c) symbols -> ('b, 'a -> 'c) symbols
  type 'a symbol_attribution =
  | SA : ('a, 'b) symbols * 'b -> 'a symbol_attribution
  val string_of_symbol_attribution : 'a symbol_attribution -> string
  type rule
  val rule : 'a n -> 'a symbol_attribution -> rule
  val symbol_attribution_opt : 'a n -> rule -> 'a symbol_attribution option
  type usymbol = UNT of nonterminal | UT of terminal
  type urule = nonterminal * usymbol list
  val urule : rule -> urule
  val string_of_rule : rule -> string
  type rules = rule list
  val string_of_rules : string -> rules -> string
end

module MAKE(S : Symbols) : Types with type 'a n = 'a S.n
                                 and type 'a token = 'a S.token
                                 and type lexeme = S.lexeme = struct
  include S

  (* universal type for terminals *)
  module T = struct
    type 'a t = 'a token
    let to_string = string_of_token
  end
  module TBox = Univ.Box.MAKE(T)
  type terminal = TBox.t
  let terminal t = TBox.box t
  let string_of_terminal t = TBox.to_string t

  (* universal type for non-terminals *)
  module N = struct
    type 'a t = 'a n
    let to_string = string_of_n
  end
  module NTBox = Univ.Box.MAKE(N)
  type nonterminal = NTBox.t
  let nonterminal t = NTBox.box t
  let string_of_nonterminal t = NTBox.to_string t
    
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

  type 'a symbol_attribution =
  | SA : ('a, 'b) symbols * 'b -> 'a symbol_attribution

  let string_of_symbol_attribution = function
    | SA (symbols, attribution) -> string_of_symbols symbols

  (* create universal type for rules *)
  module W = struct
    type 'a key = 'a n
    type 'a value = 'a symbol_attribution
    let string_of_key = string_of_n
    let string_of_value = string_of_symbol_attribution
    type ('a, 'b) conv = {
      key : 'c . 'c key -> 'a;
      value : 'c . 'c value -> 'b;
    }
  end
  module Boxed = Univ.Witnessed.MAKE(W)
  type rule = Boxed.t
  let rule n symbol_attribution = Boxed.box n symbol_attribution

  type usymbol = UNT of nonterminal | UT of terminal
  type urule = nonterminal * usymbol list
  let urule rule =
    let rec map_symbols : type a b . (a, b) symbols -> usymbol list = function
      | Ret -> []
      | Arg (sym, syms) ->
        let sym =
          match sym with
          | NT n -> UNT (nonterminal n)
          | T t -> UT (terminal t)
        in
        sym :: (map_symbols syms)
    in
    let conv = {
      W.key = nonterminal;
      W.value = (function SA (symbols, _) -> map_symbols symbols);
    }
    in
    Boxed.conv conv rule

  (* return Some symbol_attribution if the non-terminal matches the rule *)
  let symbol_attribution_opt n rule = Boxed.value_opt n rule

  let string_of_rule rule =
    let (n, symbols) = Boxed.as_strings rule
    in
    n ^ " := " ^ symbols

  type rules = rule list

  let string_of_rules prefix rules =
    let sep2 = "\n" ^ prefix in
    let (_, str) =
      List.fold_left
        (fun (sep, str) rule -> (sep2, (str ^ sep ^ (string_of_rule rule))))
        (prefix, "")
        rules
    in
    str
end
