structure Mlex=
   struct
    structure UserDeclarations =
      struct
type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\000\000\000\000\000\000\000\000\022\022\000\000\000\022\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\000\058\000\000\000\056\000\055\054\053\052\051\050\049\048\
\\000\000\000\000\000\000\000\000\000\000\000\047\045\044\042\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\041\000\040\000\000\
\\000\007\035\027\007\023\018\007\007\014\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\009\007\007\007\006\004\003\000\000\
\\000"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\010\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\011\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\012\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (12, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\013\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\017\007\007\007\007\007\007\007\015\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\016\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\022\022\000\000\000\022\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\021\007\007\007\007\007\007\007\007\019\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\020\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\022\022\000\000\000\022\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\021\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\022\022\000\000\000\022\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\022\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\024\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\025\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\026\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\028\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\029\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\030\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\031\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\032\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\033\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\034\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\036\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (36, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\037\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\038\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\007\
\\000\007\007\007\007\007\007\007\007\007\007\039\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\000\000\000\000\000\
\\000"
),
 (42, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (56, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\057\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (58, 
"\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\059\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\058\
\\058"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 29)], trans = 0},
{fin = [], trans = 4},
{fin = [(N 2)], trans = 0},
{fin = [(N 27)], trans = 0},
{fin = [(N 86)], trans = 7},
{fin = [(N 86)], trans = 7},
{fin = [(N 86)], trans = 9},
{fin = [(N 86)], trans = 10},
{fin = [(N 86)], trans = 11},
{fin = [(N 86)], trans = 12},
{fin = [(N 57),(N 86)], trans = 7},
{fin = [(N 86)], trans = 14},
{fin = [(N 86)], trans = 15},
{fin = [(N 47),(N 86)], trans = 7},
{fin = [(N 71),(N 86)], trans = 7},
{fin = [(N 86),(N 93)], trans = 18},
{fin = [(N 86)], trans = 19},
{fin = [(N 51),(N 86)], trans = 7},
{fin = [(N 86),(N 93)], trans = 21},
{fin = [(N 93)], trans = 22},
{fin = [(N 86)], trans = 23},
{fin = [(N 86)], trans = 24},
{fin = [(N 86)], trans = 25},
{fin = [(N 62),(N 86)], trans = 7},
{fin = [(N 86)], trans = 27},
{fin = [(N 86)], trans = 28},
{fin = [(N 86)], trans = 29},
{fin = [(N 86)], trans = 30},
{fin = [(N 86)], trans = 31},
{fin = [(N 86)], trans = 32},
{fin = [(N 86)], trans = 33},
{fin = [(N 80),(N 86)], trans = 7},
{fin = [(N 86)], trans = 35},
{fin = [(N 86)], trans = 36},
{fin = [(N 86)], trans = 37},
{fin = [(N 86)], trans = 38},
{fin = [(N 68),(N 86)], trans = 7},
{fin = [(N 37)], trans = 0},
{fin = [(N 35)], trans = 0},
{fin = [(N 10)], trans = 42},
{fin = [(N 8)], trans = 0},
{fin = [(N 17)], trans = 0},
{fin = [(N 15)], trans = 45},
{fin = [(N 13)], trans = 0},
{fin = [(N 43)], trans = 0},
{fin = [(N 19)], trans = 0},
{fin = [(N 39)], trans = 0},
{fin = [(N 23)], trans = 0},
{fin = [(N 41)], trans = 0},
{fin = [(N 25)], trans = 0},
{fin = [(N 21)], trans = 0},
{fin = [(N 33)], trans = 0},
{fin = [(N 31)], trans = 0},
{fin = [], trans = 56},
{fin = [(N 5)], trans = 0},
{fin = [], trans = 58},
{fin = [(N 90)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  10 => let val yytext=yymktext() in Tokens.GT(yypos, yypos + size yytext) end
| 13 => let val yytext=yymktext() in Tokens.LE(yypos, yypos + size yytext) end
| 15 => let val yytext=yymktext() in Tokens.LT(yypos, yypos + size yytext) end
| 17 => let val yytext=yymktext() in Tokens.EQ(yypos, yypos + size yytext) end
| 19 => let val yytext=yymktext() in Tokens.DIVIDE(yypos, yypos + size yytext) end
| 2 => let val yytext=yymktext() in Tokens.OR(yypos, yypos + size yytext) end
| 21 => let val yytext=yymktext() in Tokens.TIMES(yypos, yypos + size yytext) end
| 23 => let val yytext=yymktext() in Tokens.MINUS(yypos, yypos + size yytext) end
| 25 => let val yytext=yymktext() in Tokens.PLUS(yypos, yypos + size yytext) end
| 27 => let val yytext=yymktext() in Tokens.LBRACE(yypos, yypos + size yytext) end
| 29 => let val yytext=yymktext() in Tokens.RBRACE(yypos, yypos + size yytext) end
| 31 => let val yytext=yymktext() in Tokens.LPAREN(yypos, yypos + size yytext) end
| 33 => let val yytext=yymktext() in Tokens.RPAREN(yypos, yypos + size yytext) end
| 35 => let val yytext=yymktext() in Tokens.LBRACK(yypos, yypos + size yytext) end
| 37 => let val yytext=yymktext() in Tokens.RBRACK(yypos, yypos + size yytext) end
| 39 => let val yytext=yymktext() in Tokens.DOT(yypos, yypos + size yytext) end
| 41 => let val yytext=yymktext() in Tokens.COMMA(yypos, yypos + size yytext) end
| 43 => let val yytext=yymktext() in Tokens.SEMICOLON(yypos, yypos + size yytext) end
| 47 => let val yytext=yymktext() in Tokens.INT_TYPE(yypos,yypos+ size yytext) end
| 5 => let val yytext=yymktext() in Tokens.AND(yypos, yypos + size yytext) end
| 51 => let val yytext=yymktext() in Tokens.FOR(yypos, yypos + size yytext) end
| 57 => let val yytext=yymktext() in Tokens.WHILE(yypos, yypos + size yytext) end
| 62 => let val yytext=yymktext() in Tokens.ELSE(yypos, yypos + size yytext) end
| 68 => let val yytext=yymktext() in Tokens.BREAK(yypos, yypos + size yytext) end
| 71 => let val yytext=yymktext() in Tokens.IF(yypos, yypos + size yytext) end
| 8 => let val yytext=yymktext() in Tokens.GE(yypos, yypos + size yytext) end
| 80 => let val yytext=yymktext() in Tokens.CONTINUE(yypos, yypos +size yytext) end
| 86 => let val yytext=yymktext() in Tokens.ID(yytext, yypos, yypos + size yytext) end
| 90 => let val yytext=yymktext() in Tokens.STRING(yytext, yypos, yypos + size yytext) end
| 93 => (continue())
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end