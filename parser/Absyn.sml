structure Ast =
struct

datatype Arithematic_Operator = Plus 
			| Minus 
			| Times
			| Divide;

datatype Relational_operator = EQ
				| NEQ
				| LT
				| GT
				| GE
				| LE;
				
datatype Logical_operator = AND
			| OR;

(* The abstract syntax for expressions *)

datatype Expr  = Const of int
		| Id of string
        | Bin_Op   of Expr * Arithematic_Operator * Expr
		| Log_op   of Expr * Logical_operator * Expr
		| Rel_op   of Expr * Relational_operator * Expr;

(* meaning of binary operators *)
(*
datatype Stm = Var of string 
		| Exp_abs of Expr               (*doubt*)
*)

datatype Stms = If of Expr * Stms list
		| Ifelse of Expr*Stms list*Stms list
		| While of Expr * Stms list
		| Assign of string * Expr
		

fun plus (x,y) = Bin_Op (x, Plus, y)
fun minus (x,y) = Bin_Op (x, Minus, y)
fun times(x,y)= Bin_Op (x, Times, y)
fun divide (x,y) = Bin_Op (x, Divide, y)		

fun and_op (x,y) = Log_op (x , AND, y)
fun or_op (x,y) = Log_op (x , OR, y)

fun equality (x,y) = Rel_op (x , EQ, y)
fun not_equal (x,y) = Rel_op (x , NEQ, y)
fun greater_then (x,y) = Rel_op (x , GT, y)
fun less_then (x,y) = Rel_op (x , LT, y)
fun greater_or_equal (x,y) = Rel_op (x , GE, y)
fun smaller_or_equal (x,y) = Rel_op (x , LE, y)
(*
fun IF a b = If (a,b)
fun IFELSE a b c = Ifelse(a, b, c)
fun arith_Op Plus  x y = x + y
  | arith_Op Minus x y = x - y
  | arith_Op Times   x y = x * y
  | arith_Op Divide x y = x / y;

fun logical_op And x y = x && y
  | logical_op OR x y = x || y;

fun relation_op EEQ x y = x == y
  | relation_op NEQ x y = x != y
  | relation_op LE x y = x <= y
  | relation_op GE x y = x >= y
  | relation_op GT x y = x > y
  | relation_op LT x y = x < y ;
  
fun exprDenote (Const x)       = x
  | exorDenote (Id x) 	       = x
  | exprDenote (Bin_op (x,oper,y)) = arith_op oper (exprDenote x) (exprDenote y)
  | exprDenote (Log_op (x,oper,y)) = logical_op oper (exprDenote x) (exprDenote y)
  | exprDenote (Rel_op (x,oper,y)) = relational_op oper (exprDenote x) (exprDenote y);	

(* Conversion to strings *)

fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Times   = "*"
  | binOpToString Divide = "/";
  
fun logOpToSring And = "&&"
  | logOpToString OR = "||";

fun relOpToString EEQ = "=="
  | relOpToString NEQ = "!="
  | relOpToString GT = ">"
  | relOpToString LT = "<"
  | relOpToString GE = ">="
  | relOpToString LE = "<="
*)
end

