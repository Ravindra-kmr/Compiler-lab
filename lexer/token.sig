
signature C_TOKENS =
sig
type num 
type token

val BREAK:  num * num -> token
val CONTINUE: num * num -> token
val FOR:  num * num -> token
val WHILE:  num * num -> token
val ELSE:  num * num -> token
val ELSEIF:  num * num -> token
val IF:  num * num -> token
val OR:  num * num -> token
val AND:  num * num -> token
val GE:  num * num -> token
val GT:  num * num -> token
val LE:  num * num -> token
val LT:  num * num -> token
val NEQ:  num * num -> token
val EQ:  num * num -> token
val DIVIDE:  num * num -> token
val TIMES:  num * num -> token
val MINUS:  num * num -> token
val PLUS:  num * num -> token
val DOT:  num * num -> token
val RBRACE:  num * num -> token
val LBRACE:  num * num -> token
val RBRACK:  num * num -> token
val LBRACK:  num * num -> token
val INTEGER_TYPE : num * num -> token
val RPAREN:  num * num -> token
val LPAREN:  num * num -> token
val SEMICOLON:  num * num -> token
val STRING: (string) *  num * num -> token
val INT: (int) *  num * num -> token
val ID: (string) *  num * num -> token
val EOF:  num * num -> token
end
