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

module TA = struct
  type _ token =
  | TInt : int  token
  | TAdd : unit token
  | TMul : unit token
  | TEof : unit token

  let string_of_token : type a . a token -> string = function
    | TInt -> "TInt"
    | TAdd -> "TAdd"
    | TMul -> "TMul"
    | TEof -> "TEof"

  type _ attrib =
  | ANil : unit attrib
  | AInt : int -> int attrib

  let string_of_attrib : type a . a attrib -> string = function
    | ANil -> ""
    | AInt i -> string_of_int i

  let value_of_attrib : type a . a attrib -> a = function
    | ANil -> ()
    | AInt i -> i
end

module Lexer' = Lexer.MAKE(TA)
module Lexer = struct
  include TA
  include (Lexer' : module type of Lexer' with type 'a token := 'a TA.token
					  and type 'a attrib := 'a TA.attrib)
end

(* test: 3*2+1 *)
let lexemes = Lexer.([
  lexeme TInt (AInt 3);
  lexeme TMul ANil;
  lexeme TInt (AInt 2);
  lexeme TAdd ANil;
  lexeme TInt (AInt 1);
  lexeme TEof ANil;
])

let () = Printf.printf "Input = '%s'\n" (Lexer.string_of_lexemes lexemes)

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

    module Assoc = Eq.MAKE(struct type 'a t = 'a n end)
    let n_equal n1 n2 = Assoc.equal n1 n2
    let string_of_n : type a . a n -> string = function
      | Start -> "Start"
      | AddExpr -> "AddExpr"
      | MulExpr -> "MulExpr"

    type 'a token = 'a TA.token
    let string_of_token = TA.string_of_token
    type 'a attrib = 'a TA.attrib
    let string_of_attrib = TA.string_of_attrib
    type lexeme = Lexer.lexeme
    let string_of_lexemes = Lexer.string_of_lexemes
    let attrib_opt = Lexer.attrib_opt
    let value_of_attrib = Lexer.value_of_attrib
  end
  open Symbols

  module Types = Types.MAKE(Symbols)
  include Types

  (*
    Start   := AddExpr TEof
    AddExpr := MulExpr + AddExpr
    AddExpr := MulExpr
    MulExpr := TInt * MulExpr
    MulExpr := TInt
  *)
  open TA
  let rule_start = Rule (P (Start, (NT AddExpr) ^^^ (T TEof) ^^^ ret),
                         A (fun e _ -> e))
  let rule_add1 = Rule (P (AddExpr, (NT MulExpr) ^^^ (T TAdd) ^^^ (NT AddExpr) ^^^ ret),
                        A (fun e1 _ e2 -> Add (e1, e2)))
  let rule_add2 = Rule (P (AddExpr, (NT MulExpr) ^^^ ret),
                        A (fun e -> e))
  let rule_mul1 = Rule (P (MulExpr, (T TInt) ^^^ (T TMul) ^^^ (NT MulExpr) ^^^ ret),
                        A (fun i _ e -> Mul (Val i, e)))
  let rule_mul2 = Rule (P (MulExpr, (T TInt) ^^^ ret),
                        A (fun i -> Val i))
  type start_nt = expression
  type start_attrib = expression -> unit -> expression
  let start = rule_start
  let rules = rule_start @@@ rule_add1 @@@ rule_add2 @@@ rule_mul1 @@@ rule_mul2 @@@ Nil
  let () = Printf.printf "rules =\n%s\n" (string_of_rules " " rules)

  type grammar = Grammar : rules * 'a n -> grammar
end

module Parser = Parser.MAKE(Grammar)
let (exp, _) = Parser.parse 0 Grammar.start lexemes
let () = Printf.printf "Parsed = '%s'\n" (Grammar.Symbols.string_of_expression exp)
