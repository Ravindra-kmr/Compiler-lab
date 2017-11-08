functor gramLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : gram_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*structure A = Absyn*)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
<<<<<<< HEAD
\\001\000\002\000\010\000\007\000\009\000\008\000\008\000\026\000\007\000\
\\027\000\006\000\029\000\005\000\000\000\
\\001\000\002\000\014\000\000\000\
\\001\000\002\000\015\000\000\000\
\\001\000\002\000\023\000\003\000\022\000\006\000\021\000\019\000\020\000\000\000\
\\001\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\
\\011\000\036\000\012\000\035\000\013\000\034\000\014\000\033\000\
\\015\000\032\000\016\000\031\000\000\000\
\\001\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\
\\020\000\059\000\000\000\
\\001\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\
\\025\000\045\000\000\000\
\\001\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\
\\025\000\062\000\000\000\
\\001\000\006\000\043\000\000\000\
\\001\000\017\000\030\000\018\000\029\000\024\000\028\000\000\000\
\\001\000\017\000\030\000\018\000\029\000\024\000\042\000\000\000\
\\001\000\019\000\046\000\000\000\
\\001\000\019\000\060\000\000\000\
\\001\000\019\000\068\000\000\000\
\\001\000\020\000\065\000\000\000\
\\001\000\020\000\066\000\000\000\
\\001\000\020\000\070\000\000\000\
\\001\000\023\000\012\000\000\000\
\\001\000\023\000\013\000\000\000\
\\001\000\025\000\061\000\000\000\
\\001\000\030\000\016\000\000\000\
\\001\000\030\000\025\000\000\000\
\\001\000\030\000\026\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\028\000\067\000\000\000\
=======
\\001\000\002\000\009\000\006\000\008\000\025\000\007\000\026\000\006\000\
\\028\000\005\000\000\000\
\\001\000\002\000\013\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\020\000\019\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\007\000\036\000\008\000\035\000\
\\010\000\034\000\011\000\033\000\012\000\032\000\013\000\031\000\
\\014\000\030\000\015\000\029\000\000\000\
\\001\000\004\000\038\000\005\000\037\000\007\000\036\000\008\000\035\000\
\\021\000\056\000\000\000\
\\001\000\009\000\014\000\000\000\
\\001\000\009\000\023\000\000\000\
\\001\000\010\000\028\000\016\000\027\000\017\000\026\000\021\000\025\000\000\000\
\\001\000\010\000\028\000\016\000\027\000\017\000\026\000\023\000\040\000\000\000\
\\001\000\018\000\057\000\000\000\
\\001\000\019\000\061\000\000\000\
\\001\000\020\000\011\000\000\000\
\\001\000\022\000\012\000\000\000\
\\001\000\022\000\042\000\000\000\
\\001\000\022\000\063\000\000\000\
\\001\000\023\000\060\000\000\000\
\\001\000\023\000\065\000\000\000\
\\001\000\024\000\010\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\027\000\062\000\000\000\
\\072\000\000\000\
\\073\000\004\000\038\000\005\000\037\000\007\000\036\000\008\000\035\000\000\000\
\\074\000\004\000\038\000\005\000\037\000\007\000\036\000\008\000\035\000\000\000\
\\075\000\010\000\028\000\000\000\
\\076\000\010\000\028\000\016\000\027\000\000\000\
>>>>>>> da2c6c98f5e4f013b310a51efb18a63844a8d723
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\017\000\030\000\000\000\
\\083\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\000\000\
\\084\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\000\000\
\\085\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\000\000\
\\086\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\000\000\
\\087\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\000\000\
\\088\000\004\000\040\000\005\000\039\000\009\000\038\000\010\000\037\000\000\000\
\\089\000\009\000\038\000\010\000\037\000\000\000\
\\090\000\009\000\038\000\010\000\037\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\"
val actionRowNumbers =
"\001\000\001\000\024\000\026\000\
\\018\000\019\000\002\000\003\000\
\\021\000\025\000\004\000\004\000\
\\022\000\023\000\004\000\010\000\
\\045\000\005\000\004\000\049\000\
\\048\000\047\000\011\000\009\000\
\\004\000\007\000\012\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\004\000\
\\004\000\004\000\004\000\006\000\
\\013\000\020\000\008\000\032\000\
\\001\000\034\000\033\000\038\000\
\\040\000\037\000\036\000\039\000\
\\035\000\044\000\043\000\042\000\
\\041\000\046\000\001\000\031\000\
\\030\000\015\000\016\000\028\000\
\\027\000\014\000\001\000\017\000\
\\029\000\000\000"
val gotoT =
"\
\\001\000\069\000\002\000\002\000\003\000\001\000\000\000\
\\002\000\009\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\017\000\005\000\016\000\006\000\015\000\000\000\
\\004\000\017\000\005\000\016\000\006\000\022\000\000\000\
\\000\000\
\\000\000\
\\004\000\025\000\005\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\039\000\005\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\042\000\005\000\016\000\000\000\
\\000\000\
\\000\000\
\\004\000\017\000\005\000\016\000\006\000\045\000\000\000\
\\004\000\017\000\005\000\016\000\006\000\046\000\000\000\
\\004\000\047\000\005\000\016\000\000\000\
\\004\000\048\000\005\000\016\000\000\000\
\\004\000\049\000\005\000\016\000\000\000\
\\004\000\050\000\005\000\016\000\000\000\
\\004\000\051\000\005\000\016\000\000\000\
\\004\000\052\000\005\000\016\000\000\000\
\\004\000\053\000\005\000\016\000\000\000\
\\004\000\054\000\005\000\016\000\000\000\
\\004\000\055\000\005\000\016\000\000\000\
\\004\000\056\000\005\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\061\000\003\000\001\000\000\000\
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
\\002\000\062\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\067\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 70
val numrules = 26
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
type pos = INT
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | STRING_CONST of  (string)
 | INT_CONST of  (int) | ID of  (string)
end
type svalue = MlyValue.svalue
type result = unit
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
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT_CONST"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
<<<<<<< HEAD
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
  | (T 26) => "IF"
  | (T 27) => "ELSE"
  | (T 28) => "NIL"
  | (T 29) => "ASSIGN"
=======
  | (T 5) => "INTEGER_TYPE"
  | (T 6) => "TIMES"
  | (T 7) => "DIVIDE"
  | (T 8) => "EQ"
  | (T 9) => "EEQ"
  | (T 10) => "NEQ"
  | (T 11) => "LT"
  | (T 12) => "LE"
  | (T 13) => "GT"
  | (T 14) => "GE"
  | (T 15) => "AND"
  | (T 16) => "OR"
  | (T 17) => "LBRACE"
  | (T 18) => "RBRACE"
  | (T 19) => "LBRACK"
  | (T 20) => "RBRACK"
  | (T 21) => "LPARAN"
  | (T 22) => "RPARAN"
  | (T 23) => "SEMICOLON"
  | (T 24) => "WHILE"
  | (T 25) => "IF"
  | (T 26) => "ELSE"
  | (T 27) => "NIL"
>>>>>>> da2c6c98f5e4f013b310a51efb18a63844a8d723
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
<<<<<<< HEAD
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16)
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 4) $$ (T 3) $$ (T 0)end
=======
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 4) $$ (T 3) $$ (T 0)end
>>>>>>> da2c6c98f5e4f013b310a51efb18a63844a8d723
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, statements1left, statements1right)) :: rest671))
 => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 0, ( result, statements1left, statements1right), 
rest671)
end
|  ( 1, ( ( _, ( _, _, statements1right)) :: ( _, ( _, statement1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 1, ( result, statement1left, statements1right), 
rest671)
end
|  ( 2, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID ()
 in ( LrTable.NT 1, ( result, NIL1left, NIL1right), rest671)
end
|  ( 3, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: _ :: _ :: _ :: ( _,
 ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, WHILE1left, RBRACE1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: _ :: _ :: _ :: ( _,
 ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (
)
 in ( LrTable.NT 2, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RBRACE2right)) :: _ :: _ :: _ :: _ :: _ :: _ ::
 _ :: _ :: _ :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, IF1left, RBRACE2right), rest671)
end
|  ( 6, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: _ :: ( _, ( _, 
INT1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, INT1left, SEMICOLON1right), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: _ :: ( _, ( _, 
STRING1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, STRING1left, SEMICOLON1right), rest671)

end
|  ( 8, ( ( _, ( _, _, SEMICOLON1right)) :: _ :: _ :: ( _, ( _, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, ID1left, SEMICOLON1right), rest671)
end
|  ( 9, ( ( _, ( _, _, comp_exp2right)) :: _ :: ( _, ( _, 
comp_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (
)
 in ( LrTable.NT 5, ( result, comp_exp1left, comp_exp2right), rest671)

end
|  ( 10, ( ( _, ( _, _, comp_exp2right)) :: _ :: ( _, ( _, 
comp_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (
)
 in ( LrTable.NT 5, ( result, comp_exp1left, comp_exp2right), rest671)

end
|  ( 11, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 12, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 13, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 14, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 15, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 16, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 5, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 17, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 18, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 19, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 20, ( ( _, ( _, _, arith_exp2right)) :: _ :: ( _, ( _, 
arith_exp1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 ()
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp2right), 
rest671)
end
|  ( 21, ( ( _, ( _, exp1left, exp1right)) :: rest671)) => let val  
result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, exp1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( _, LBRACE1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 23, ( ( _, ( _, ID1left, ID1right)) :: rest671)) => let val  
result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( _, INT_CONST1left, INT_CONST1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, INT_CONST1left, INT_CONST1right), 
rest671)
end
|  ( 25, ( ( _, ( _, STRING_CONST1left, STRING_CONST1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, STRING_CONST1left, STRING_CONST1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : gram_TOKENS =
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
<<<<<<< HEAD
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
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
=======
fun EEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LPARAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun RPARAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
>>>>>>> da2c6c98f5e4f013b310a51efb18a63844a8d723
ParserData.MlyValue.VOID,p1,p2))
end
end
