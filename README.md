# Compiler-lab
Using ml-yacc and ml-lex making a compiler for c programming language.
Tools needed:

$sudo apt update
$sudo apt install smlnj ml-yacc ml-lex ml-burg ml-lpt
$sudo apt install ml-ulex

How to run:
1. $ cd Compiler-lab/parse
2. $ sml
3. -CM.make("sources.cm");
4. -Parser.parse "test";

FIles:
c_lex.lex - lexer file for a subset of c language in ml-lex.
parser.grm - grammer file for a subset of c language.
Absyn.sml - datatype file for the parser.grm.
test - a test file containing c language syntax code for checking the compiler.

