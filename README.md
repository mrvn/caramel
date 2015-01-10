caramel
=======

Ocaml like toy programming language


I'm building caramel from scratch without any existing tools. For this I have a few reasons:
  * learning by doing - Understanding how each step in a compiler works.
  * type safety - I'm using ocaml GADTs to increase type safety for the lexer and parser. Afaik no lexer/parser generator does that yet.
  * becoming self hosting - Once the compiler can produce some code it will be easier to rewrite it in caramel.

Progress
--------

  * Regular expressions: done
  * Universal containers: Witnessed, Box
  * Lexer: done
  * Parser: simple recursive parser
  * Parser: finish SLR(k) parser with positions
  * tokenize: begining of the language
  * compiler: tokenize and parse from stdin
  * compiler: print first and follow from LR parser
