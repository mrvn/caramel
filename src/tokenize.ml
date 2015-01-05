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
module TA = struct
  type ident = string

  type upper_ident = string

  type prefix = string

  type infix = string

  type _ token =
  | TIdent : ident token
  | TUpperIdent : upper_ident token
  | TInt : int token
  | TString : string token
  | TChar : char token
  | TPrefix : prefix token
  | TInfix0 : infix token
  | TInfix1 : infix token
  | TInfix2 : infix token
  | TInfix3 : infix token
  | TInfix4 : infix token
  | TLParen : unit token
  | TRParen : unit token
  | TIf : unit token
  | TElse : unit token
  | TLBrace : unit token
  | TRBrace : unit token
  | TAsterisk : unit token

  | TEof : unit token

  let keyword_associations =
    [ ("if", TIf);
      ("else", TElse);
      
      ("(", TLParen);
      (")", TRParen);
      ("*", TAsterisk);
      ("{", TLBrace);
      ("}", TRBrace);
    ]

  let string_of_token : type a . a token -> string = function
    | TIdent -> "TIdent"
    | TUpperIdent -> "TUpperIdent"
    | TInt -> "TInt"
    | TString -> "TString"
    | TChar -> "TChar"
    | TPrefix -> "TPrefix"
    | TInfix0 -> "TInfix0"
    | TInfix1 -> "TInfix1"
    | TInfix2 -> "TInfix2"
    | TInfix3 -> "TInfix3"
    | TInfix4 -> "TInfix4"
    | TLParen -> "("
    | TRParen -> ")"
    | TIf -> "if"
    | TElse -> "else"
    | TLBrace -> "{"
    | TRBrace -> "}"
    | TAsterisk -> "*"

    | TEof -> "<eof>"

  (* FIXME: use equal
  (* for debuging, tracing *)
  type box = Box : 'a token -> box
  let string_of_token : type a . a token -> string = function t ->
    let assoc_r b l =
      let rec loop = function
        | [] -> raise Not_found
        | (a, maybe_b)::l_rest -> 
          if Box b = Box maybe_b then a
          else loop l_rest in
      loop l in
    try 
      assoc_r t keyword_associations
    with Not_found -> 
      match t with
        | TIdent -> "TIdent"
        | TUpperIdent -> "TUpperIdent"
        | TString -> "TString"
        | TInt -> "TInt"
        | TChar -> "TChar"
        | TEof -> "TEof"
        | _ -> "Operator"      
  *)
      
  type _ attrib =
  | ANil : unit attrib
  | AInt : int -> int attrib
  | AString : string -> string attrib
  | AChar : char -> char attrib
  | AIdent : ident -> ident attrib
  | AUpperIdent : upper_ident -> upper_ident attrib
  | APrefix : prefix -> prefix attrib
  | AInfix : infix -> infix attrib

  let string_of_attrib : type a . a attrib -> string = function
    | ANil -> ""
    | AInt i -> string_of_int i
    | AString s -> s
    | AChar c -> Regexp.string_of_char ['\\'] c
    | AIdent s -> s
    | AUpperIdent s -> s
    | APrefix s -> s
    | AInfix s -> s

  let value_of_attrib : type a . a attrib -> a = function
    | ANil -> ()
    | AInt i -> i
    | AString s -> s
    | AChar c -> c
    | AIdent s -> s
    | AUpperIdent s -> s
    | APrefix s -> s
    | AInfix s -> s
end

module LT' = LexerTypes.MAKE(TA)
module LT = struct
  include TA
  include (LT' : module type of LT' with type 'a token := 'a TA.token
                                    and type 'a attrib := 'a TA.attrib)
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
