(* Regular expression module
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

type 'a regexp =
  | Null
  | Epsilon
  | Symbol of 'a
  | Concat of 'a regexp * 'a regexp
  | Alternate of 'a regexp * 'a regexp
  | Repeat of 'a regexp
  | OneOf of 'a list
  | NotOf of 'a list

let string_of_char escaped c =
  if c = '\n'
  then "\\n"
  else if c = '\r'
  then "\\r"
  else if c = '\t'
  then "\\t"
  else if c = ' '
  then "<space>"
  else if (Char.code c < 32) || (Char.code c >= 127)
  then Printf.sprintf "\\%03d" (Char.code c)
  else if List.mem c escaped
  then Printf.sprintf "'\\%c'" c
  else Printf.sprintf "'%c'" c

let rec string_of_regexp = function
  | Null -> "<Null>"
  | Epsilon -> ""
  | Symbol c -> string_of_char ['\\'; '\''] c
  | Concat(e1, e2) ->
    Printf.sprintf "%s%s"
      (string_of_regexp e1)
      (string_of_regexp e2)
  | Alternate(e1, e2) ->
    Printf.sprintf "(%s|%s)"
      (string_of_regexp e1)
      (string_of_regexp e2)
  | Repeat e -> Printf.sprintf "(%s)*" (string_of_regexp e)
  | OneOf list ->
    (List.fold_left
       (fun acc c -> acc ^ string_of_char ['\\'; '\''; '-'; '^'; '['; ']'] c)
       "["
       list)
    ^ "]"
  | NotOf list ->
    (List.fold_left
       (fun acc c -> acc ^ string_of_char ['\\'; '\''; '-'; '^'; '['; ']'] c)
       "[^"
       list)
    ^ "]"

(* Construction and normalization *)

let epsilon = Epsilon
let symbol x = Symbol(x)
let concat r1 r2 =
  match (r1, r2) with
    | (Null, _ )
    | (_, Null) -> Null
    | (Epsilon, _) -> r2
    | (_, Epsilon) -> r1
    | _ -> Concat(r1, r2)
let alternate r1 r2 =
  match (r1, r2) with
    | (Null, _) -> r2
    | (_, Null) -> r1
    | _ -> Alternate(r1, r2)
let repeat = function
  | Null
  | Epsilon -> Epsilon
  | r -> Repeat(r)

let repeat_one r = concat r (repeat r)

let concat_list l = List.fold_left concat Epsilon l

let alternate_list l = List.fold_left alternate Null l

let rec int_range i1 i2 =
  if i1 = i2
  then [i1]
  else i1::(int_range (i1+1) i2)

let char_range c1 c2 =
  List.map Char.chr (int_range (Char.code c1) (Char.code c2))

module Symbols = struct
  type 'a n =
  | Start : 'a regexp n
  | Regexp : 'a regexp n
  | ExpQuant : ('a regexp -> 'a regexp) n
  | Exp : 'a regexp n
  | Sequence : 'a regexp n
  | Range : 'a list n
  | Alternate : 'a regexp n
  | More : ('a regexp -> 'a regexp) n

  let string_of_n : type a . a n -> string = function
  | Start -> "Start"
  | Regexp -> "Regexp"
  | ExpQuant -> "ExpQuant"
  | Exp -> "Exp"
  | Sequence -> "Sequence"
  | Range -> "Range"
  | Alternate -> "Alternate"
  | More -> "More"
      
  type 'a token =
  | TChar : char token
  | TLParen : unit token
  | TRParen : unit token
  | TLBracket : unit token
  | TRBracket : unit token
  | TCaret : unit token
  | TDash : unit token
  | TStar : unit token
  | TPipe : unit token
  | TPlus : unit token
  | TOpt : unit token
  | TEof : unit token
      
  type 'a attrib =
  | ANil : unit attrib
  | AChar : char -> char attrib

  type lexeme =
  | Lexeme : 'a token * 'a attrib -> lexeme

  let string_of_token : type a . a token -> string = function
    | TChar -> "TChar"
    | TLParen -> "("
    | TRParen -> ")"
    | TLBracket -> "["
    | TRBracket -> "]"
    | TCaret -> "^"
    | TDash -> "-"
    | TStar -> "*"
    | TPipe -> "|"
    | TPlus -> "+"
    | TOpt -> "?"
    | TEof -> "<eof>"

  let string_of_attrib : type a . a attrib -> string = function
    | ANil -> ""
    | AChar c -> string_of_char ['\\'; '\''] c

  let string_of_lexeme (Lexeme (token, attrib)) =
    match string_of_attrib attrib with
    | "" -> string_of_token token
    | attrib -> attrib
    
  let string_of_lexemes lexemes =
    let (_, str) =
      List.fold_left
	(fun (sep, str) (_, lexeme) ->
	  (" ", (str ^ sep ^ (string_of_lexeme lexeme))))
	("", "")
	lexemes
    in
    str

  module TAssoc = Eq.MAKE(struct type 'a t = 'a token end)
  let equal = TAssoc.equal

  let cast : type a b . (a, b) Eq.t -> a attrib -> b attrib = fun Eq.Refl x -> x

  let attrib_opt : type a . a token -> lexeme -> a attrib option
  = fun token (Lexeme (t, v)) ->
    match equal t token with
    | None -> None
    | Some equality -> Some (cast equality v)

  let value_of_attrib : type a . a attrib -> a = function
    | ANil -> ()
    | AChar c -> c

  module NAssoc = Eq.MAKE(struct type 'a t = 'a n end)
  let n_equal = NAssoc.equal
end

module T = Types.MAKE(Symbols)

module Grammar = struct
  include T
  open Symbols
  type start_nt = char regexp
  type start_attrib = char regexp -> char regexp
  let start = Rule (P (Start, NT Regexp ^^^ Ret),
		    A (fun e -> e))

    (* Regexp    := Sequence <eof>
     * Sequence  := Exp ExpQuant Sequence | ""
     * ExpQuant  := '*' | '+' | '?' | ""
     * Exp       := <char> | '-' | '[' Range ']' | '[' '^' Range ']'
     *            | '(' Alternate ')'
     * Range     := '|' Range | '*' Range | '+' Range | '?' Range
     *            | '(' Range | ')' Range | <char> '-' <char>
     *            | <char> Range | ""
     * Alternate := Sequence More
     * More      := '|' Alternate | ""
     *)
       
  let rules =
    start
    @@@ Rule (P (Regexp, NT Sequence ^^^ T TEof ^^^ Ret),
	      A (fun e _ -> e))

    @@@ Rule (P (Sequence, NT Exp ^^^ NT ExpQuant ^^^ NT Sequence ^^^ Ret),
	      A (fun e1 fn e2 -> concat (fn e1) e2))
    @@@ Rule (P (Sequence, Ret),
	      A Epsilon)

    @@@ Rule (P (ExpQuant, T TStar ^^^ Ret),
	      A (fun _ -> fun e -> repeat e))
    @@@ Rule (P (ExpQuant, T TPlus ^^^ Ret),
	      A (fun _ -> fun e -> concat e (repeat e)))
    @@@ Rule (P (ExpQuant, T TOpt ^^^ Ret),
	      A (fun _ -> fun e -> alternate Epsilon e))
    @@@ Rule (P (ExpQuant, Ret),
	      A (fun e -> e))

    @@@ Rule (P (Exp, T TChar ^^^ Ret),
	      A (fun c -> symbol c))
    @@@ Rule (P (Exp, T TDash ^^^ Ret),
	      A (fun _ -> Symbol '-'))
    @@@ Rule (P (Exp, T TLBracket ^^^ NT Range ^^^ T TRBracket ^^^ Ret),
	      A (fun _ list _ -> OneOf list))
    @@@ Rule (P (Exp, T TLBracket ^^^ T TCaret ^^^ NT Range ^^^ T TRBracket
                      ^^^ Ret),
       A (fun _ _ list _ -> NotOf list))
    @@@ Rule (P (Exp, T TLParen ^^^ NT Alternate ^^^ T TRParen ^^^ Ret),
	      A (fun _ e _ -> e))

    @@@ Rule (P (Range, T TPipe ^^^ NT Range ^^^ Ret),
	      A (fun _ list -> '|' :: list))
    @@@ Rule (P (Range, T TStar ^^^ NT Range ^^^ Ret),
	      A (fun _ list -> '*' :: list))
    @@@ Rule (P (Range, T TPlus ^^^ NT Range ^^^ Ret),
	      A (fun _ list -> '+' :: list))
    @@@ Rule (P (Range, T TOpt ^^^ NT Range ^^^ Ret),
	      A (fun _ list -> '?' :: list))
    @@@ Rule (P (Range, T TLParen ^^^ NT Range ^^^ Ret),
	      A (fun _ list -> '(' :: list))
    @@@ Rule (P (Range, T TRParen ^^^ NT Range ^^^ Ret),
	      A (fun _ list -> ')' :: list))
    @@@ Rule (P (Range, T TChar ^^^ T TDash ^^^ T TChar ^^^ NT Range ^^^ Ret),
	      A (fun c1 _ c2 list ->
		let r = char_range c1 c2
		in
		r @ list))
    @@@ Rule (P (Range, T TChar ^^^ NT Range ^^^ Ret),
	      A (fun c list -> c :: list))
    @@@ Rule (P (Range, Ret),
	      A [])

    @@@ Rule (P (Alternate, NT Sequence ^^^ NT More ^^^ Ret),
	      A (fun e fn -> fn e))

    @@@ Rule (P (More, T TPipe ^^^ NT Alternate ^^^ Ret),
	      A (fun _ e2 -> fun e1 -> alternate e1 e2))
    @@@ Rule (P (More, Ret),
	      A (fun e -> e))

    @@@ Nil
end

module Parser = Parser.MAKE(Grammar)

let explode s =
  let len = String.length s in
  let rec loop acc = function
    | -1 -> acc
    | i -> loop (s.[i] :: acc) (i - 1)
  in
  loop [] (len - 1)

let lexer list =
  Symbols.(
    let rec loop pos acc list =
      let next = Pos.next pos in
      match list with
      | [] -> List.rev ((pos, Lexeme (TEof, ANil)) :: acc)
      | '('::rest -> loop next ((pos, Lexeme (TLParen, ANil)) :: acc) rest
      | ')'::rest -> loop next ((pos, Lexeme (TRParen, ANil)) :: acc) rest
      | '['::rest -> loop next ((pos, Lexeme (TLBracket, ANil)) :: acc) rest
      | ']'::rest -> loop next ((pos, Lexeme (TRBracket, ANil)) :: acc) rest
      | '^'::rest -> loop next ((pos, Lexeme (TCaret, ANil)) :: acc) rest
      | '-'::rest -> loop next ((pos, Lexeme (TDash, ANil)) :: acc) rest
      | '*'::rest -> loop next ((pos, Lexeme (TStar, ANil)) :: acc) rest
      | '|'::rest -> loop next ((pos, Lexeme (TPipe, ANil)) :: acc) rest
      | '+'::rest -> loop next ((pos, Lexeme (TPlus, ANil)) :: acc) rest
      | '?'::rest -> loop next ((pos, Lexeme (TOpt, ANil)) :: acc) rest
      | '\\'::c::rest -> loop next ((pos, Lexeme (TChar, AChar c)) :: acc) rest
      | c::rest -> loop next ((pos, Lexeme (TChar, AChar c)) :: acc) rest
    in
    loop Pos.start [] list)

let make str =
  Printf.printf "Regexp.make '%s'\n%!" str;
  let lexemes = lexer (explode str) in
  let (re, _) = Parser.parse 0 Grammar.start lexemes
  in
  re

let make_string str =
  concat_list (List.map symbol (explode str))

(* Operations *)

let is_null r = (r = Null)

let rec accepts_empty = function
  | Null
  | Symbol(_)
  | OneOf(_)
  | NotOf(_) -> false
  | Epsilon
  | Repeat(_) -> true
  | Concat(r1, r2) -> accepts_empty(r1) && accepts_empty(r2)
  | Alternate(r1, r2) -> accepts_empty(r1) || accepts_empty(r2)

let rec after_symbol symbol = function
  | Null
  | Epsilon -> Null
  | Symbol(symbol') ->
    if symbol = symbol'
    then Epsilon
    else Null
  | Concat(r1, r2) ->
    let after_1 = concat (after_symbol symbol r1) r2 in
    let after_2 =
      if accepts_empty r1
      then after_symbol symbol r2
      else Null
    in
    alternate after_1 after_2
  | Alternate(r1, r2) ->
    alternate (after_symbol symbol r1) (after_symbol symbol r2)
  | Repeat(r1) -> concat (after_symbol symbol r1) (Repeat(r1))
  | OneOf(list) -> if List.mem symbol list then Epsilon else Null
  | NotOf(list) -> if List.mem symbol list then Null else Epsilon
