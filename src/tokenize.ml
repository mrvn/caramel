(* tokenizer
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

module Re = Regexp

(* regexp helper *)
let bindigit = "[01]"
let binprefix = "0(b|B)"
let octdigit = "[0-7]"
let octprefix =  "0(o|O)"
let digit = "[0-9]"
let hexdigit = "[0-9A-Fa-f]"
let hexprefix =  "0(x|X)"
let sign = "(|-)"
let backslash = "\\\\"

let repeat_one str = str ^ str ^ "*"

let upper_letter = "[A-Z]"
let lower_letter = "[a-z]"
let letter = "[A-Za-z]"
let ident_rest = "(" ^ letter ^ "|" ^ digit ^ "|_|')*"
let literal_char_char = "[^\\']"
let literal_string_char = "[^\\\"]"
let escape_char_char = backslash ^ "[\\'ntbr]"
let escape_string_char = backslash ^ "[\\\"ntbr]"
let dec_char = backslash ^ digit ^ digit ^ digit
let op_rest = "[!$%&*+\\-./:<=>?@\\^|~]*"

(* Regexps *)
let blank = Re.make "[ \t\n\r\012]+"
let eof = Re.make "\004"
let ident = Re.make (lower_letter ^ ident_rest)
let upper_ident = Re.make (upper_letter ^ ident_rest)
let integer_literal =
  Re.make (
    sign
    ^ "(" ^ repeat_one digit
    ^ "|" ^ hexprefix ^ repeat_one hexdigit
    ^ "|" ^ octprefix ^ repeat_one octdigit
    ^ "|" ^ binprefix ^ repeat_one bindigit
    ^ ")")
let string_literal =
  Re.make (
    "\"(" ^ escape_string_char
    ^ "|" ^ dec_char
    ^ "|" ^ literal_string_char
    ^ ")*\"")
let char_literal_literal = Re.make ("'" ^ literal_char_char ^ "'")
let char_escape_literal = Re.make ("'" ^ escape_char_char ^ "'")
let char_dec_literal = Re.make ("'" ^ dec_char ^ "'")
let comment_start = Re.make "\\(\\*"
let infix_symbol_0 = Re.make ("[=<>|&$]" ^ op_rest)
let infix_symbol_1 = Re.make ("[@\\^]" ^ op_rest)
let infix_symbol_2 = Re.make ("[+\\-]" ^ op_rest)
let infix_symbol_3 = Re.make ("[*/%]" ^ op_rest)
let infix_symbol_4 = Re.make ("\\*\\*" ^ op_rest)
let prefix_symbol = Re.make ("[!?~]" ^ op_rest)

(* helper *)
let char_for_backslash c =
  match c with
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code l =
  let c = 100 * (Char.code (List.hd l) - 48) +
    10 * (Char.code (List.hd (List.tl l)) - 48) +
    (Char.code (List.hd (List.tl (List.tl l))) - 48) in  
  Char.chr (c land 0xFF)

let list_to_string l =
  let length = List.length l in
  let string = String.create length in
  let rec loop i rest =
    if i = length
    then string
    else
      (String.set string i (List.hd rest);
       loop (i + 1) (List.tl rest))
  in
  loop 0 l

let skip_comment level stream =
  let rec loop level c1 = function
    | [] -> (* FIXME: report pos *) raise (Failure "unbalanced comment")
    | (pos, c2) :: stream ->
      match (c1, c2) with 
      | ('(', '*') -> loop (level + 1) ' ' stream
      | ('*', ')') ->
        if level = 1
        then stream
        else loop (level - 1) ' ' stream
      | _ -> loop level c2 stream
  in
    loop level ' ' stream


(* Lexer types *)
module LT' = LexerTypes.MAKE(Token)
module LT = struct
  include Token
  include (LT' : module type of LT' with type 'a token := 'a Token.token
                                    and type 'a attrib := 'a Token.attrib)
end

module StringLexer = struct
  module TA = struct
    type _ token = unit
    type _ attrib = Char : char -> char attrib
    let string_of_token : type a . a token -> string = function _ -> "T"
    let string_of_attrib : type a . a attrib -> string
    = function Char c -> Regexp.string_of_char ['\\'] c
    let value_of_attrib : type a . a attrib -> a = function Char c -> c
  end

  module LT' = LexerTypes.MAKE(TA)
  module LT = struct
    include TA
    include (LT' : module type of LT' with type 'a token := 'a TA.token
                                      and type 'a attrib := 'a TA.attrib)
  end
    
  module Rules = struct
    include LT
    type symbol = char
    type action =
      Pos.t * symbol list * symbol Input.t
      -> Pos.t * lexeme * symbol Input.t
    type rule = symbol Regexp.regexp * action
    type scan_one = symbol Input.t -> Pos.t * lexeme * symbol Input.t

    let make c = lexeme () (Char c)

    let specs scan_one =
      [
        (Re.make "\"",
         function (start, lexeme, stream) ->
           (start, make '#', stream));
        (Re.make escape_string_char,
         function (start, lexeme, stream) ->
           (start, make (char_for_backslash (List.hd (List.tl lexeme))),
            stream));
        (Re.make dec_char,
         function (start, lexeme, stream) ->
           (start, make (char_for_decimal_code (List.tl lexeme)), stream));
        (Re.make literal_string_char,
         function (start, lexeme, stream) ->
           (start, make (List.hd lexeme), stream));
      ]
  end

  module Lexer = Lexer.MAKE(Rules)
  include Lexer
  
  let scan chars =
    Lexer.scan (List.map (fun c -> (Pos.nil, c)) chars)
end
  
(* Lexer rules *)
module Rules = struct
  include LT

  (* new stuff *)
  type symbol = char
  type action =
    Pos.t * symbol list * symbol Input.t
    -> Pos.t * lexeme * symbol Input.t
  type rule = symbol Regexp.regexp * action
  type scan_one = symbol Input.t -> Pos.t * lexeme * symbol Input.t

  let specs scan_one =
    [
      (eof,
       function (start, _, stream) ->
         (start, lexeme TEof ANil, stream));
      (blank,
       function (start, _, stream) ->
         scan_one stream);
    ]
    @
      (List.map
         (function (keyword, token) ->
           (Re.make_string keyword,
            (function (start, _, stream) ->
              (start, lexeme token ANil, stream))))
         keyword_associations)
    @
      [ (ident,
         function (start, lexemes, stream) ->
           (start,
            lexeme TIdent (AIdent (list_to_string lexemes)),
            stream));
        (upper_ident,
         function (start, lexemes, stream) ->
           (start,
            lexeme TUpperIdent (AIdent (list_to_string lexemes)),
            stream));
        (integer_literal,
         function (start, lexemes, stream) ->
           (start,
            lexeme TInt  (AInt (int_of_string (list_to_string lexemes))),
            stream));
        (string_literal,
         function (start, lexemes, stream) ->
           (start,
            lexeme
              TString
              (AString
                 (list_to_string
                    (* kludge *)
                    (List.rev
                       (List.tl
                          (List.rev
			     (* map lexemes to char list *)
                             (List.map
                                (fun (pos, lexeme) ->
				  match StringLexer.attrib_opt () lexeme with
				  | None -> assert false
				  | Some attrib ->
				    StringLexer.value_of_attrib attrib)
                                (StringLexer.scan (List.tl lexemes)))))))),
            stream));
        (char_literal_literal,
         function (start, lexemes, stream) ->
           (start,
            lexeme TChar (AChar (List.hd (List.tl lexemes))),
            stream));
        (char_escape_literal,
         function (start, lexemes, stream) ->
           (start,
            lexeme
              TChar
              (AChar
                 (char_for_backslash
                    (List.hd (List.tl (List.tl lexemes))))),
            stream));
        (char_dec_literal,
         function (start, lexemes, stream) ->
           (start,
            lexeme
              TChar
              (AChar
                 (char_for_decimal_code (List.tl (List.tl lexemes)))),
            stream));
        (comment_start,
         function (start, _, stream) ->
           scan_one (skip_comment 1 stream));
        (prefix_symbol,
         function (start, lexemes, stream) ->
           (start,
            lexeme TPrefix (APrefix (list_to_string lexemes)),
            stream));
        (infix_symbol_0,
         function (start, lexemes, stream) ->
           (start,
            lexeme TInfix0 (AInfix (list_to_string lexemes)),
            stream));
        (infix_symbol_1,
         function (start, lexemes, stream) ->
           (start,
            lexeme TInfix1 (AInfix (list_to_string lexemes)),
            stream));
        (infix_symbol_2,
         function (start, lexemes, stream) ->
           (start,
            lexeme TInfix2 (AInfix (list_to_string lexemes)),
            stream));
        (infix_symbol_4,
         function (start, lexemes, stream) ->
           (start,
            lexeme TInfix4 (AInfix (list_to_string lexemes)),
            stream));
        (infix_symbol_3,
         function (start, lexemes, stream) ->
           (start,
            lexeme TInfix3 (AInfix (list_to_string lexemes)),
            stream))
      ]
end

module Lexer' = Lexer.MAKE(Rules)

include Rules
include (Lexer' : module type of Lexer' with type 'a token := 'a Rules.token
                                        and type 'a attrib := 'a Rules.attrib
                                        and type lexeme := Rules.lexeme
                                        and type symbol := Rules.symbol
                                        and type action := Rules.action
                                        and type rule := Rules.rule
                                        and type scan_one := Rules.scan_one
                                        and type ('a, 'b) lexeme_conv := ('a, 'b) Rules.lexeme_conv)
