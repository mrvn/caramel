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
| TThen : unit token
| TElse : unit token
| TLBrace : unit token
| TRBrace : unit token
| TAsterisk : unit token
| TLet : unit token
| TIn : unit token
| TEquals : unit token

| TEof : unit token

module W = struct
  type 'a key = 'a token
  type 'a value = string
  let string_of_key k = assert false
  let string_of_value v = v
end

module M = Univ.Witnessed.MAKE(W)

let keyword_associations =
  [
    ("if", TIf);
    ("then", TThen);
    ("else", TElse);
    ("let", TLet);
    ("in", TIn);
    
    ("=", TEquals);
    ("(", TLParen);
    (")", TRParen);
    ("*", TAsterisk);
    ("{", TLBrace);
    ("}", TRBrace);
  ]

let keyword_to_string = List.map (fun (s, t) -> M.box t s) keyword_associations
    
let string_of_token : type a . a token -> string = function token ->
  match M.find token keyword_to_string with
  | Some s -> s
  | None ->
    match token with
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
      
    | TEof -> "<eof>"
    | _ -> assert false

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
