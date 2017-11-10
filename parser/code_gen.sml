CM.make("sources.cm");
Control.Print.printDepth:=20;
val ast = Parser.parse "test";

fun converting_int_and_var (Ast.Id (id)) = id
	|converting_int_and_var (Ast.Const (integer)) = Int.toString integer

fun bin_op (Ast.Bin_Op (exp1, Ast.Plus, exp2)) = bin_op exp1 ^ "+" ^ bin_op exp2
	|bin_op (Ast.Bin_Op (exp1, Ast.Minus ,exp2)) = bin_op exp1 ^ "-" ^ bin_op exp2
	|bin_op (Ast.Bin_Op (exp1,Ast.Times ,exp2)) = bin_op exp1 ^ "*" ^ bin_op exp2
	|bin_op (Ast.Bin_Op (exp1,Ast.Divide ,exp2)) = bin_op exp1 ^ "/" ^ bin_op exp2
	|bin_op (Ast.Const(integer)) = (Int.toString (integer))
	|bin_op (Ast.Id(ID)) = ID
	
fun rel_op (Ast.Rel_op (exp1,Ast.EQ,exp2)) = bin_op exp1 ^ "==" ^ bin_op exp2
   |rel_op (Ast.Rel_op (exp1,Ast.NEQ ,exp2)) = bin_op exp1 ^ "!=" ^ bin_op exp2
   |rel_op (Ast.Rel_op (exp1,Ast.LT,exp2)) = bin_op exp1 ^ "<" ^ bin_op exp2
   |rel_op (Ast.Rel_op (exp1,Ast.GT,exp2)) = bin_op exp1 ^ ">" ^ bin_op exp2
   |rel_op (Ast.Rel_op (exp1,Ast.LE,exp2)) = bin_op exp1 ^"<=" ^ bin_op exp2
   |rel_op (Ast.Rel_op (exp1,Ast.GE,exp2)) = bin_op exp1 ^ ">=" ^ bin_op exp2
   	

fun log_op (Ast.Log_op (exp1,Ast.AND, exp2)) = rel_op exp1 ^ "&&" ^ rel_op exp2
    |log_op (Ast.Log_op (exp1,Ast.OR, exp2)) = rel_op exp1 ^ "||" ^ rel_op exp2

fun statementlist [] = ""
  | statementlist (x::xs) = (statement x) (statementlist xs)
    
and statement (Ast.While (condition, stmlist)) = "while" ^"( " ^ condition ^" )" ^"{ " ^"\n" ^ statementlist stmlist ^"}" ^"\n"
  |statement (Ast.Assign (Ast.Id(ID),exp)) = ID ^ "=" ^ bin_op exp ^ ";\n"
  |statement (Ast.Initialize (Ast.Id(ID), exp)) = "int " ^ ID ^ " = " ^ bin_op exp ^ ";\n"
  |statement (Ast.If (exp1, stmlist , exp2)) = "if " ^ "( " ^ rel_op exp1 ^ " )" ^ "{\n" ^statementlist stmlist ^ "\n}"
  |statement (Ast.Ifelse (exp1, stmlist , stmlist1)) = "if " ^ "( " ^ rel_op exp1 ^ " )" ^ "{\n" ^statementlist stmlist ^ "\n}" ^ "else{\n" ^ statementlist stmlist1 ^ "}\n" 
  
fun writefile (filename , content) = let val fd = TextIO.openOut filename
        				val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        				val _ = TextIO.closeOut fd
    				  in () 
    				  end
 
val filecontent = statementlist ast
val t = writefile (output , ast );
