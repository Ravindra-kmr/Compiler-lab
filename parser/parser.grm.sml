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
\\001\000\002\000\015\000\003\000\014\000\006\000\013\000\022\000\012\000\
\\025\000\011\000\027\000\010\000\028\000\009\000\029\000\008\000\
\\030\000\007\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\018\000\051\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\018\000\052\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\018\000\056\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\021\000\050\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\023\000\049\000\000\000\
\\001\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\
\\023\000\053\000\000\000\
\\001\000\017\000\029\000\000\000\
\\001\000\017\000\030\000\000\000\
\\001\000\017\000\054\000\000\000\
\\001\000\022\000\031\000\000\000\
\\058\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\016\000\017\000\000\000\
\\059\000\000\000\
\\060\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\000\000\
\\064\000\000\000\
\\065\000\020\000\016\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\000\000\
\\069\000\000\000\
\\070\000\007\000\026\000\008\000\025\000\000\000\
\\071\000\007\000\026\000\008\000\025\000\000\000\
\\072\000\000\000\
\\073\000\000\000\
\\074\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\000\000\
\\075\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\009\000\024\000\010\000\023\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\014\000\019\000\015\000\018\000\000\000\
\\076\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\011\000\022\000\012\000\021\000\013\000\020\000\014\000\019\000\000\000\
\\077\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\078\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\079\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\080\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\
\\011\000\022\000\012\000\021\000\013\000\020\000\014\000\019\000\000\000\
\\081\000\004\000\028\000\005\000\027\000\007\000\026\000\008\000\025\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\"
val actionRowNumbers =
"\001\000\019\000\018\000\012\000\
\\017\000\008\000\009\000\016\000\
\\015\000\011\000\001\000\014\000\
\\013\000\036\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\006\000\005\000\
\\029\000\028\000\033\000\035\000\
\\032\000\031\000\034\000\030\000\
\\027\000\026\000\025\000\024\000\
\\002\000\003\000\007\000\023\000\
\\037\000\022\000\021\000\010\000\
\\001\000\004\000\020\000\000\000"
val gotoT =
"\
\\002\000\055\000\003\000\004\000\004\000\003\000\005\000\002\000\
\\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\004\000\030\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\004\000\031\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\032\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\033\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\034\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\035\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\036\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\037\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\038\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\039\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\040\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\041\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\042\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\043\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\044\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\045\000\005\000\002\000\006\000\001\000\000\000\
\\003\000\004\000\004\000\046\000\005\000\002\000\006\000\001\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\004\000\004\000\053\000\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 56
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
datatype svalue = VOID | ntVOID of unit | STRING of  (string)
 | INT of  (int) | ID of  (string)
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
  | (T 2) => "INT"
  | (T 3) => "PLUS"
  | (T 4) => "MINUS"
  | (T 5) => "STRING"
  | (T 6) => "TIMES"
  | (T 7) => "DIVIDE"
  | (T 8) => "EQ"
  | (T 9) => "NEQ"
  | (T 10) => "LT"
  | (T 11) => "LE"
  | (T 12) => "GT"
  | (T 13) => "GE"
  | (T 14) => "AND"
  | (T 15) => "OR"
  | (T 16) => "LBRACE"
  | (T 17) => "RBRACE"
  | (T 18) => "DOT"
  | (T 19) => "LBRACK"
  | (T 20) => "RBRACK"
  | (T 21) => "LPARAN"
  | (T 22) => "RPARAN"
  | (T 23) => "SEMICOLON"
  | (T 24) => "WHILE"
  | (T 25) => "FOR"
  | (T 26) => "BREAK"
  | (T 27) => "CONTINUE"
  | (T 28) => "IF"
  | (T 29) => "ELSE"
  | (T 30) => "ELSEIF"
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
of  ( 0, ( ( _, ( _, exp1left, exp1right)) :: rest671)) => let val  
result = MlyValue.ntVOID ()
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, INT1left, INT1right), rest671)
end
|  ( 2, ( ( _, ( _, STRING1left, STRING1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, STRING1left, STRING1right), rest671)
end
|  ( 3, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 4, ( ( _, ( _, CONTINUE1left, CONTINUE1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, CONTINUE1left, CONTINUE1right), rest671)

end
|  ( 5, ( ( _, ( _, arith_exp1left, arith_exp1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, arith_exp1left, arith_exp1right), 
rest671)
end
|  ( 6, ( ( _, ( _, comp_exp1left, comp_exp1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, comp_exp1left, comp_exp1right), rest671)

end
|  ( 7, ( ( _, ( _, lvalue1left, lvalue1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: _ :: _ :: _ :: ( _,
 ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, WHILE1left, RBRACE1right), rest671)
end
|  ( 9, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: ( _, ( _, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: ( _, ( _, 
ELSE1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, ELSE1left, RBRACE1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RPARAN1right)) :: _ :: ( _, ( _, LPARAN1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 3, ( result, LPARAN1left, RPARAN1right), rest671)
end
|  ( 12, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 2, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( _, _, exp2right)) :: _ :: ( _, ( _, exp1left, _)) ::
 rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 4, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( _, ID1left, ID1right)) :: rest671)) => let val  
result = MlyValue.ntVOID ()
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RBRACK1right)) :: _ :: _ :: ( _, ( _, 
lvalue1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID ()
 in ( LrTable.NT 5, ( result, lvalue1left, RBRACK1right), rest671)
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
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT i,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.STRING i,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
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
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSEIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
end
end
