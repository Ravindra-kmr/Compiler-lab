functor CLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : C_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Ast


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\014\000\000\000\
\\001\000\002\000\020\000\003\000\019\000\000\000\
\\001\000\002\000\021\000\000\000\
\\001\000\002\000\067\000\000\000\
\\001\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\
\\011\000\033\000\012\000\032\000\013\000\031\000\014\000\030\000\
\\015\000\029\000\016\000\028\000\000\000\
\\001\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\
\\024\000\072\000\000\000\
\\001\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\
\\025\000\041\000\000\000\
\\001\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\
\\025\000\057\000\000\000\
\\001\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\
\\025\000\059\000\000\000\
\\001\000\017\000\027\000\018\000\026\000\024\000\025\000\000\000\
\\001\000\017\000\027\000\018\000\026\000\024\000\039\000\000\000\
\\001\000\017\000\027\000\018\000\026\000\025\000\065\000\000\000\
\\001\000\019\000\042\000\000\000\
\\001\000\019\000\056\000\000\000\
\\001\000\019\000\066\000\000\000\
\\001\000\019\000\073\000\000\000\
\\001\000\020\000\061\000\000\000\
\\001\000\020\000\063\000\000\000\
\\001\000\020\000\070\000\000\000\
\\001\000\020\000\075\000\000\000\
\\001\000\023\000\011\000\000\000\
\\001\000\023\000\012\000\000\000\
\\001\000\023\000\013\000\000\000\
\\001\000\031\000\015\000\000\000\
\\001\000\031\000\023\000\000\000\
\\001\000\031\000\038\000\000\000\
\\001\000\031\000\069\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\002\000\009\000\007\000\008\000\026\000\007\000\027\000\006\000\
\\028\000\005\000\000\000\
\\080\000\000\000\
\\081\000\029\000\064\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\017\000\027\000\000\000\
\\088\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\000\000\
\\089\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\000\000\
\\090\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\000\000\
\\091\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\000\000\
\\092\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\000\000\
\\093\000\004\000\037\000\005\000\036\000\009\000\035\000\010\000\034\000\000\000\
\\094\000\009\000\035\000\010\000\034\000\000\000\
\\095\000\009\000\035\000\010\000\034\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\"
val actionRowNumbers =
"\030\000\030\000\028\000\021\000\
\\022\000\023\000\001\000\024\000\
\\029\000\002\000\003\000\002\000\
\\025\000\002\000\010\000\049\000\
\\005\000\051\000\050\000\026\000\
\\011\000\002\000\007\000\013\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\014\000\008\000\036\000\
\\030\000\038\000\037\000\042\000\
\\044\000\041\000\040\000\043\000\
\\039\000\048\000\047\000\046\000\
\\045\000\009\000\030\000\035\000\
\\017\000\002\000\018\000\032\000\
\\012\000\031\000\015\000\004\000\
\\030\000\027\000\019\000\002\000\
\\033\000\006\000\016\000\030\000\
\\020\000\034\000\000\000"
val gotoT =
"\
\\001\000\074\000\002\000\002\000\003\000\001\000\000\000\
\\002\000\008\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\016\000\005\000\015\000\006\000\014\000\000\000\
\\000\000\
\\004\000\016\000\005\000\015\000\006\000\020\000\000\000\
\\000\000\
\\004\000\022\000\005\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\038\000\005\000\015\000\000\000\
\\000\000\
\\000\000\
\\004\000\016\000\005\000\015\000\006\000\041\000\000\000\
\\004\000\016\000\005\000\015\000\006\000\042\000\000\000\
\\004\000\043\000\005\000\015\000\000\000\
\\004\000\044\000\005\000\015\000\000\000\
\\004\000\045\000\005\000\015\000\000\000\
\\004\000\046\000\005\000\015\000\000\000\
\\004\000\047\000\005\000\015\000\000\000\
\\004\000\048\000\005\000\015\000\000\000\
\\004\000\049\000\005\000\015\000\000\000\
\\004\000\050\000\005\000\015\000\000\000\
\\004\000\051\000\005\000\015\000\000\000\
\\004\000\052\000\005\000\015\000\000\000\
\\004\000\053\000\005\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\056\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\058\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\016\000\005\000\015\000\006\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\066\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\069\000\005\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\072\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 75
val numrules = 24
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | STRING_CONST of  (string)
 | INT_CONST of  (int) | ID of  (string) | comp_exp of  (Ast.Expr)
 | exp of  (Ast.Expr) | arith_exp of  (Ast.Expr)
 | statement of  (Ast.Stms) | statements of  (Ast.Stms list)
 | start of  (Ast.Stms list)
end
type svalue = MlyValue.svalue
type result = Ast.Stms list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT_CONST"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "STRING_CONST"
  | (T 6) => "INT"
  | (T 7) => "STRING"
  | (T 8) => "TIMES"
  | (T 9) => "DIVIDE"
  | (T 10) => "EQ"
  | (T 11) => "NEQ"
  | (T 12) => "LT"
  | (T 13) => "LE"
  | (T 14) => "GT"
  | (T 15) => "GE"
  | (T 16) => "AND"
  | (T 17) => "OR"
  | (T 18) => "LBRACE"
  | (T 19) => "RBRACE"
  | (T 20) => "LBRACK"
  | (T 21) => "RBRACK"
  | (T 22) => "LPARAN"
  | (T 23) => "RPARAN"
  | (T 24) => "SEMICOLON"
  | (T 25) => "WHILE"
  | (T 26) => "FOR"
  | (T 27) => "IF"
  | (T 28) => "ELSE"
  | (T 29) => "NIL"
  | (T 30) => "ASSIGN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 4) $$ (T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statements statements, statements1left, 
statements1right)) :: rest671)) => let val  result = MlyValue.start (
statements)
 in ( LrTable.NT 0, ( result, statements1left, statements1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.statements statements, _, statements1right))
 :: ( _, ( MlyValue.statement statement, statement1left, _)) :: 
rest671)) => let val  result = MlyValue.statements (
statement::statements)
 in ( LrTable.NT 1, ( result, statement1left, statements1right), 
rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.statements ([])
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements, _, _)) :: _ :: _ :: ( _, ( MlyValue.comp_exp comp_exp, _,
 _)) :: _ :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result
 = MlyValue.statement (Ast.While(comp_exp, statements))
 in ( LrTable.NT 2, ( result, WHILE1left, RBRACE1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements, _, _)) :: _ :: _ :: ( _, ( MlyValue.comp_exp comp_exp, _,
 _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (Ast.If(comp_exp,statements))
 in ( LrTable.NT 2, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RBRACE2right)) :: ( _, ( MlyValue.statements 
statements2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.statements 
statements1, _, _)) :: _ :: _ :: ( _, ( MlyValue.comp_exp comp_exp, _,
 _)) :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (Ast.Ifelse(comp_exp,statements1,statements2))
 in ( LrTable.NT 2, ( result, IF1left, RBRACE2right), rest671)
end
|  ( 6, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.statements 
statements, _, _)) :: _ :: _ :: ( _, ( MlyValue.arith_exp arith_exp2,
 _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.comp_exp comp_exp, _, _)) ::
 _ :: ( _, ( MlyValue.arith_exp arith_exp1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, FOR1left, _)) :: rest671)) =>
 let val  result = MlyValue.statement (
Ast.For((ID,arith_exp1),comp_exp,(ID,arith_exp2),statements))
 in ( LrTable.NT 2, ( result, FOR1left, RBRACE1right), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.arith_exp 
arith_exp, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, 
INT1left, _)) :: rest671)) => let val  result = MlyValue.statement (
Ast.Assign(ID,arith_exp))
 in ( LrTable.NT 2, ( result, INT1left, SEMICOLON1right), rest671)
end
|  ( 8, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.arith_exp 
arith_exp, _, _)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.statement (
Ast.Assign(ID,arith_exp))
 in ( LrTable.NT 2, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.comp_exp comp_exp2, _, comp_exp2right)) :: _
 :: ( _, ( MlyValue.comp_exp comp_exp1, comp_exp1left, _)) :: rest671)
) => let val  result = MlyValue.comp_exp (
Ast.and_op(comp_exp1,comp_exp2))
 in ( LrTable.NT 5, ( result, comp_exp1left, comp_exp2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.comp_exp comp_exp2, _, comp_exp2right)) ::
 _ :: ( _, ( MlyValue.comp_exp comp_exp1, comp_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.or_op(comp_exp1,comp_exp2))
 in ( LrTable.NT 5, ( result, comp_exp1left, comp_exp2right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.equality(arith_exp1,arith_exp2))
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 12, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.less_then(arith_exp1,arith_exp2))
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.smaller_or_equal(arith_exp1,arith_exp2))
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.greater_or_equal(arith_exp1,arith_exp2))
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.not_equal(arith_exp1,arith_exp2))
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 16, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.comp_exp (
Ast.greater_then(arith_exp1,arith_exp2))
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.arith_exp (
Ast.plus(arith_exp1,arith_exp2))
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.arith_exp (
Ast.minus(arith_exp1,arith_exp2))
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.arith_exp (
Ast.times(arith_exp1,arith_exp2))
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.arith_exp arith_exp2, _, arith_exp2right))
 :: _ :: ( _, ( MlyValue.arith_exp arith_exp1, arith_exp1left, _)) :: 
rest671)) => let val  result = MlyValue.arith_exp (
Ast.divide(arith_exp1,arith_exp2))
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp, exp1left, exp1right)) :: rest671))
 => let val  result = MlyValue.arith_exp (exp)
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp (Ast.Id(ID))
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.INT_CONST INT_CONST, INT_CONST1left, 
INT_CONST1right)) :: rest671)) => let val  result = MlyValue.exp (
Ast.Const(INT_CONST))
 in ( LrTable.NT 4, ( result, INT_CONST1left, INT_CONST1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : C_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID i,p1,p2))
fun INT_CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT_CONST i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING_CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.STRING_CONST i,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LPARAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun RPARAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
end
end
