(* main entry for the compiler
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

let input = Input.from_stdin ()
let lexemes =
  try
    Tokenize.scan input
  with Tokenize.Scan_error(pos) ->
    Printf.printf "Scan error at %s\n" (Pos.string_of_t pos);
    exit 1
(*  
(* test: 3*2+1 *)
let lexemes = Lexer.([
  lexeme TInt (AInt 3);
  lexeme TMul ANil;
  lexeme TInt (AInt 2);
  lexeme TAdd ANil;
  lexeme TInt (AInt 1);
  lexeme TEof ANil;
])
*)
let () = Printf.printf "Input = '%s'\n" (Tokenize.string_of_lexemes lexemes)

module Grammar = struct
  module Symbols = struct
    type expression =
    | Val of int
    | Add of expression * expression
    | Mul of expression * expression

    let rec string_of_expression = function
      | Val i -> string_of_int i
      | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (string_of_expression e1) (string_of_expression e2)
      | Mul (e1, e2) -> Printf.sprintf "(%s * %s)" (string_of_expression e1) (string_of_expression e2)

    type _ n =
    | Start : expression n
    | AddExpr : expression n
    | MulExpr : expression n
    | ValExpr : expression n

    let string_of_n : type a . a n -> string = function
      | Start -> "Start"
      | AddExpr -> "AddExpr"
      | MulExpr -> "MulExpr"
      | ValExpr -> "ValExpr"

    module N = struct
      type 'a t = 'a n
      let to_string = string_of_n
    end
    module Assoc = Eq.MAKE(N)
    let n_equal n1 n2 = Assoc.equal n1 n2

    type 'a token = 'a Tokenize.token
    let string_of_token = Tokenize.string_of_token
    type 'a attrib = 'a Tokenize.attrib
    let string_of_attrib = Tokenize.string_of_attrib
    type lexeme = Tokenize.lexeme
    let string_of_lexemes = Tokenize.string_of_lexemes
    let attrib_opt = Tokenize.attrib_opt
    let value_of_attrib = Tokenize.value_of_attrib
  end
  open Symbols

  module Types = Types.MAKE(Symbols)
  include Types

  (*
    Start   := AddExpr TEof
    AddExpr := MulExpr + AddExpr
    AddExpr := MulExpr
    MulExpr := ValExpr * MulExpr
    MulExpr := ValExpr
    ValExpr := TInt
    ValExpr := TLParen AddExp TRParen
  *)
  open Tokenize
  let rule_start = Rule (P (Start, (NT AddExpr) ^^^ (T TEof) ^^^ ret),
                         A (fun e _ -> e))
  let rule_add1 = Rule (P (AddExpr, (NT MulExpr) ^^^ (T TInfix2) ^^^ (NT AddExpr) ^^^ ret),
                        A (fun e1 _ e2 -> Add (e1, e2)))
  let rule_add2 = Rule (P (AddExpr, (NT MulExpr) ^^^ ret),
                        A (fun e -> e))
  let rule_mul1 = Rule (P (MulExpr, (NT ValExpr) ^^^ (T TAsterisk) ^^^ (NT MulExpr) ^^^ ret),
                        A (fun e1 _ e2 -> Mul (e1, e2)))
  let rule_mul2 = Rule (P (MulExpr, (NT ValExpr) ^^^ ret),
                        A (fun e -> e))
  let rule_val1 = Rule (P (ValExpr, (T TInt) ^^^ ret),
                        A (fun i -> Val i))
  let rule_val2 = Rule (P (ValExpr, (T TLParen) ^^^ (NT AddExpr) ^^^ (T TRParen) ^^^ ret),
                        A (fun _ e _ -> e))
  type start_nt = expression
  type start_attrib = expression -> unit -> expression
  let start = rule_start
  let rules = rule_start @@@ rule_add1 @@@ rule_add2 @@@ rule_mul1 @@@ rule_mul2 @@@ rule_val1 @@@ rule_val2 @@@ Nil
  let () = Printf.printf "rules =\n%s\n" (string_of_rules " " rules)

end

module Parser = Parser.MAKE(Grammar)
let (exp, _) = Parser.parse 0 Grammar.start lexemes
let () = Printf.printf "Parsed = '%s'\n" (Grammar.Symbols.string_of_expression exp)



module Grammar2 = struct
  module Symbols = struct
    type expression =
    | Val of int
    | Add of expression * expression
    | Mul of expression * expression

    let rec string_of_expression = function
      | Val i -> string_of_int i
      | Add (e1, e2) -> Printf.sprintf "(%s + %s)" (string_of_expression e1) (string_of_expression e2)
      | Mul (e1, e2) -> Printf.sprintf "(%s * %s)" (string_of_expression e1) (string_of_expression e2)

    type _ n =
    | Start : expression n
    | Expr : expression n
    | AddExpr : expression n
    | MulExpr : expression n
    | ValExpr : expression n

    let string_of_n : type a . a n -> string = function
      | Start -> "Start"
      | Expr -> "Expr"
      | AddExpr -> "AddExpr"
      | MulExpr -> "MulExpr"
      | ValExpr -> "ValExpr"

    module N = struct
      type 'a t = 'a n
      let to_string = string_of_n
    end
    module Assoc = Eq.MAKE(N)
    let n_equal n1 n2 = Assoc.equal n1 n2

    type 'a token = 'a Tokenize.token
    let string_of_token = Tokenize.string_of_token
    type 'a attrib = 'a Tokenize.attrib
    let string_of_attrib = Tokenize.string_of_attrib
    type lexeme = Tokenize.lexeme
    let string_of_lexemes = Tokenize.string_of_lexemes
    let attrib_opt = Tokenize.attrib_opt
    let value_of_attrib = Tokenize.value_of_attrib
  end
  open Symbols

  module Types = ParserTypes.MAKE(Symbols)
  include Types

  (*
    Start   := Expr
    Expr    := AddExpr TEof
    AddExpr := MulExpr + AddExpr
    AddExpr := MulExpr
    MulExpr := ValExpr * MulExpr
    MulExpr := ValExpr
    ValExpr := TInt
    ValExpr := TLParen AddExp TRParen
  *)
  open Tokenize
  let rule_start = rule Start (SA (NT Expr ^^^ ret,
                                   fun e   ->  e))
  let rule_expr = rule Expr (SA (NT AddExpr ^^^ T TEof ^^^ ret,
                                 fun e          _      ->  e))
  let rule_add1 = rule AddExpr (SA (NT MulExpr ^^^ T TInfix2 ^^^ NT AddExpr ^^^ ret,
                                    fun e1         _             e2         ->  Add (e1, e2)))
  let rule_add2 = rule AddExpr (SA (NT MulExpr ^^^ ret,
                                    fun e      ->  e))
  let rule_mul1 = rule MulExpr (SA (NT ValExpr ^^^ T TAsterisk ^^^ NT MulExpr ^^^ ret,
                                    fun e1         _               e2         ->  Mul (e1, e2)))
  let rule_mul2 = rule MulExpr (SA (NT ValExpr ^^^ ret,
                                    fun e      ->  e))
  let rule_val1 = rule ValExpr (SA (T TInt ^^^ ret,
                                    fun i  ->  Val i))
  let rule_val2 = rule ValExpr (SA (T TLParen ^^^ NT AddExpr ^^^ T TRParen ^^^ ret,
                                    fun _         e              _         ->  e))
  type start = expression

  let k = 1
  let nonterminals = [nonterminal Start; nonterminal Expr; nonterminal AddExpr; nonterminal MulExpr; nonterminal ValExpr]
  let terminals = [terminal TInt; terminal TInfix2; terminal TAsterisk; terminal TLParen; terminal TRParen; terminal TEof]
  let rules = [rule_start; rule_expr; rule_add1; rule_add2; rule_mul1; rule_mul2; rule_val1; rule_val2]
  let start = Start
  let () = Printf.printf "rules =\n%s\n" (string_of_rules " " rules)
end

module Parser2 = Parser2.MAKE(Grammar2)
  (*
let (exp, _) = Parser2.parse 0 Grammar.start lexemes
let () = Printf.printf "Parsed = '%s'\n" (Grammar.Symbols.string_of_expression exp)
  *)
