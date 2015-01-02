caramel
=======

C/C++ like toy programming language


I'm building caramel from scratch without any existing tools. For this I have a few reasons:
  * learning by doing - Understanding how each step in a compiler works.
  * type safety - I'm using ocaml GADTs to increase type safety for the lexer and parser. Afaik no lexer/parser generator does that yet.
  * becoming self hosting - Once the compiler can produce some code it
  will be easier to rewrite it in caramel.

Progress
--------

  * Regular expressions: done
  * Universl containers: Witnessed
  * Lexer: type sceleton
  * Parser: simple recursive parser
  * compiler: just parser test

