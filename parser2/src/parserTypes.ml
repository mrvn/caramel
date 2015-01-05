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
  type ('a, 'b) lexeme_conv = {
    key : 'c . 'c token -> 'a;
    value : 'c . 'c attrib -> 'b;
  }
  type lexeme
  val lexeme_conv : ('a, 'b) lexeme_conv -> lexeme -> ('a * 'b)
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val string_of_lexeme : lexeme -> string
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
  type ('a, 'b) lexeme_conv = {
    key : 'c . 'c token -> 'a;
    value : 'c . 'c attrib -> 'b;
  }
  type lexeme
  val lexeme_conv : ('a, 'b) lexeme_conv -> lexeme -> ('a * 'b)
  val string_of_n : 'a n -> string
  val string_of_token : 'a token -> string
  val string_of_attrib : 'a attrib -> string
  val string_of_lexeme : lexeme -> string
  val string_of_lexemes : lexeme Input.t -> string
  val attrib_opt : 'a token -> lexeme -> 'a attrib option
  val value_of_attrib : 'a attrib -> 'a
  val n_equal : 'a n -> 'b n -> ('a, 'b) Eq.t option

  (* new *)
  type terminal
  val terminal : 'a token -> terminal
  val string_of_terminal : terminal -> string
  val terminal_of_lexeme : lexeme -> terminal
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
  type ('a, 'b) rule_conv = {
    key : 'c . 'c n -> 'a;
    value : 'c . 'c symbol_attribution -> 'b;
  }
  val rule_conv : ('a, 'b) rule_conv -> rule -> ('a * 'b)
  val symbol_attribution_opt : 'a n -> rule -> 'a symbol_attribution option
  val rules_with_lhs : nonterminal -> rule list -> rule list
  val rule_has_n : 'a n -> rule -> bool
  val rule_length : rule -> int
  type usymbol = UNT of nonterminal | UT of terminal
  type urule = nonterminal * usymbol list
  val urule : rule -> urule
  val nonterminal_of_rule : rule -> nonterminal
  val string_of_rule : rule -> string
  type rules = rule list
  val string_of_rules : string -> rules -> string
  type expr
  val expr : 'a n -> 'a -> expr
  val value_of_expr_opt : 'a n -> expr -> 'a option
  type stack_item =
  | Lexeme of lexeme
  | Expr of expr
  val rule_apply : rule -> stack_item list -> stack_item
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
  let terminal_of_lexeme lexeme =
    let conv = {
      key = terminal;
      value = (fun _ -> ());
    }
    in
    let (term, _) = lexeme_conv conv lexeme
    in
    term
    
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

  let rec symbols_length : type a b . (a, b) symbols -> int = function
    | Ret -> 0
    | Arg (_, rest) -> 1 + symbols_length rest
      
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
  end
  module Boxed = Univ.Witnessed.MAKE(W)
  type rule = Boxed.t

  type ('a, 'b) rule_conv = ('a, 'b) Boxed.conv = {
    key : 'c . 'c n -> 'a;
    value : 'c . 'c symbol_attribution -> 'b;
  }
  let rule_conv conv rule = Boxed.conv conv rule

  let rule n symbol_attribution = Boxed.box n symbol_attribution

  let rules_with_lhs (NTBox.Box n) rules =
    List.filter (Boxed.has_key n) rules

  let rule_has_n n rule = Boxed.has_key n rule

  let rule_length rule =
    let conv = {
      Boxed.key = (function _ -> ());
      Boxed.value = (function SA (syms, m_) -> symbols_length syms);
    }
    in
    let (_, len) = Boxed.conv conv rule
    in
    len
    
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
      Boxed.key = nonterminal;
      Boxed.value = (function SA (symbols, _) -> map_symbols symbols);
    }
    in
    Boxed.conv conv rule

  let nonterminal_of_rule rule =
    let (nt, _) = urule rule
    in
    nt
      
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

  module V = struct
    type 'a key = 'a n
    type 'a value = 'a
    let string_of_key = string_of_n
    let string_of_value = fun _ -> "<value>"
  end
  module U = Univ.Witnessed.MAKE(V)
  type expr = U.t
  let expr n v = U.box n v
  let value_of_expr_opt n t = U.value_opt n t

  type stack_item =
  | Lexeme of lexeme
  | Expr of expr

  let rule_apply (rule : rule) (items : stack_item list) =
    let rec loop rule items =
      match (rule, items) with
      | (Boxed.Box (n, SA (Ret, attribution)), []) -> Expr (expr n attribution)
      | (Boxed.Box (n, SA (Arg (sym, symbols), attribution)), item::items) ->
        begin
          let attribution =
            match (sym, item) with
            | (NT n, Expr expr) ->
              begin
                match value_of_expr_opt n expr with
                | Some v -> attribution v
                | None -> assert false
              end
            | (T t, Lexeme lexeme) ->
              begin
                match attrib_opt t lexeme with
                | Some a -> attribution (value_of_attrib a)
                | None -> assert false
              end
            | _ -> assert false
          in
          loop (Boxed.Box (n, (SA (symbols, attribution)))) items
        end
      | _ -> assert false
    in
    loop rule items
end
