structure Tokens : C_TOKENS =
struct

type num = int
type token = string

fun BREAK(a,b) = "BREAK   " ^ Int.toString(a)
fun CONTINUE(a,b) = "CONTINUE   " ^ Int.toString(a)
fun FOR(a,b) = "FOR   " ^ Int.toString(a)
fun WHILE(a,b) = "WHILE   " ^ Int.toString(a)
fun ELSE(a,b) = "ELSE   " ^ Int.toString(a)
fun IF(a,b) = "IF   " ^ Int.toString(a)
fun OR(a,b) = "OR   " ^ Int.toString(a)
fun AND(a,b) = "AND   " ^ Int.toString(a)
fun GE(a,b) = "GE   " ^ Int.toString(a)
fun GT(a,b) = "GT   " ^ Int.toString(a)
fun LE(a,b) = "LE   " ^ Int.toString(a)
fun LT(a,b) = "LT   " ^ Int.toString(a)
fun NEQ(a,b) = "NEQ   " ^ Int.toString(a)
fun EQ(a,b) = "EQ   " ^ Int.toString(a)
fun DIVIDE(a,b) = "DIVIDE   " ^ Int.toString(a)
fun TIMES(a,b) = "TIMES   " ^ Int.toString(a)
fun MINUS(a,b) = "MINUS   " ^ Int.toString(a)
fun PLUS(a,b) = "PLUS   " ^ Int.toString(a)
fun DOT(a,b) = "DOT   " ^ Int.toString(a)
fun RBRACE(a,b) = "RBRACE   " ^ Int.toString(a)
fun LBRACE(a,b) = "LBRACE   " ^ Int.toString(a)
fun RBRACK(a,b) = "RBRACK   " ^ Int.toString(a)
fun LBRACK(a,b) = "LBRACK   " ^ Int.toString(a)
fun RPAREN(a,b) = "RPAREN   " ^ Int.toString(a)
fun LPAREN(a,b) = "LPAREN   " ^ Int.toString(a)
fun SEMICOLON(a,b) = "SEMICOLON   " ^ Int.toString(a)
fun STRING(s,a,b) = "STRING("^s^")     " ^ Int.toString(a)
fun INT(c,a,b) = "INT("^Int.toString(c)^")   " ^ Int.toString(a)
fun ID(s,a,b) = "ID("^s^")     " ^ Int.toString(a)
fun EOF(a,b) = "EOF   " ^ Int.toString(a)
end
