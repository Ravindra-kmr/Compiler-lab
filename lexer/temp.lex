type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%

letter = [a-zA-Z];
digit = [0-9]+;
id = {letter}({letter}|{digit}|_)*;
Space = [\ \t\b\f\r]+;
quote = ["];
notQuote = [^"];

%%

"||"  => (Tokens.OR(yypos, yypos + size yytext));
"&&"  => (Tokens.AND(yypos, yypos + size yytext));
">=" => (Tokens.GE(yypos, yypos + size yytext));
">"  => (Tokens.GT(yypos, yypos + size yytext));
"<=" => (Tokens.LE(yypos, yypos + size yytext));
"<"  => (Tokens.LT(yypos, yypos + size yytext));
"="  => (Tokens.EQ(yypos, yypos + size yytext));
"!=" => (Tokens.NEQ(yypos, yypos + size yytext));
"==" => (Tokens.EEQ(yypos, yypos + size yytext));
"/"  => (Tokens.DIVIDE(yypos, yypos + size yytext));
"*"  => (Tokens.TIMES(yypos, yypos + size yytext));
"-"  => (Tokens.MINUS(yypos, yypos + size yytext));
"+"  => (Tokens.PLUS(yypos, yypos + size yytext));
"{" => (Tokens.LBRACE(yypos, yypos + size yytext));
"}" => (Tokens.RBRACE(yypos, yypos + size yytext));
"(" => (Tokens.LPAREN(yypos, yypos + size yytext));
")" => (Tokens.RPAREN(yypos, yypos + size yytext));
"[" => (Tokens.LBRACK(yypos, yypos + size yytext));
"]" => (Tokens.RBRACK(yypos, yypos + size yytext));
"." => (Tokens.DOT(yypos, yypos + size yytext));
"," => (Tokens.COMMA(yypos, yypos + size yytext));
";" => (Tokens.SEMICOLON(yypos, yypos + size yytext));

"int"  	 => (Tokens.INTEGER_TYPE(yypos,yypos+ size yytext));
"for"      => (Tokens.FOR(yypos, yypos + size yytext));
"while"    => (Tokens.WHILE(yypos, yypos + size yytext));
"else" => (Tokens.ELSE(yypos, yypos + size yytext));
"break" => (Tokens.BREAK(yypos, yypos + size yytext));
"if" => (Tokens.IF(yypos, yypos + size yytext));
"continue"  	 => (Tokens.CONTINUE(yypos, yypos +size yytext));
"elseif" =>  (Tokens.ELSEIF(yypos, yypos + size yytext));

{id} => (Tokens.ID(yytext, yypos, yypos + size yytext));
{quote}{notQuote}*{quote} => (Tokens.STRING(yytext, yypos, yypos + size yytext));
{Space} => (continue());
