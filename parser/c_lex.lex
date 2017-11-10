type pos = int
type svalue = Tokens.svalue
 
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token


val lineNum = ErrorMsg.lineNum 
val linePos = ErrorMsg.linePos 
fun err(p1,p2) = ErrorMsg.error p1 

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%%
%header (functor CLexFun(structure Tokens: C_TOKENS));
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
"="  => (Tokens.ASSIGN(yypos, yypos + size yytext));
"!=" => (Tokens.NEQ(yypos, yypos + size yytext));
"==" => (Tokens.EQ(yypos, yypos + size yytext));
"/"  => (Tokens.DIVIDE(yypos, yypos + size yytext));
"*"  => (Tokens.TIMES(yypos, yypos + size yytext));
"-"  => (Tokens.MINUS(yypos, yypos + size yytext));
"+"  => (Tokens.PLUS(yypos, yypos + size yytext));
"{" => (Tokens.LBRACE(yypos, yypos + size yytext));
"}" => (Tokens.RBRACE(yypos, yypos + size yytext));
"(" => (Tokens.LPARAN(yypos, yypos + size yytext));
")" => (Tokens.RPARAN(yypos, yypos + size yytext));
"[" => (Tokens.LBRACK(yypos, yypos + size yytext));
"]" => (Tokens.RBRACK(yypos, yypos + size yytext));
";" => (Tokens.SEMICOLON(yypos, yypos + size yytext));

"int"  	 => (Tokens.INT(yypos,yypos+ size yytext));
"for"   => (Tokens.FOR(yypos,yypos+ size yytext));
"while"    => (Tokens.WHILE(yypos, yypos + size yytext));
"else" => (Tokens.ELSE(yypos, yypos + size yytext));

"if" => (Tokens.IF(yypos, yypos + size yytext));

{digit} => (Tokens.INT_CONST(valOf(Int.fromString yytext),yypos,yypos+size yytext));
{id} => (Tokens.ID(yytext, yypos, yypos + size yytext));

{Space} => (continue());
\n({Space}*\n)* => (continue());
