structure Parser =
struct

    structure CLrVals = CLrValsFun(structure Token = LrParser.Token);
    structure CLex    = CLexFun(structure Tokens = CLrVals.Tokens);
    structure CParser = Join(
                                structure ParserData = CLrVals.ParserData
                                structure Lex = CLex
                                structure LrParser = LrParser);

    (*
    val lexer = Clex.makeLexer( fn n => valOf (TextIO.inputLine( TextIO.openIn "f" ) );
    
    
    
    *)
    
    exception CError;
    fun parse fileName = 
        let val inStream = TextIO.openIn fileName
	    fun readNext n = if TextIO.endOfStream inStream then ""
	                     else TextIO.inputN (inStream, n)
            val lexer = CParser.makeLexer readNext
            fun printError (msg,line,col) = print (fileName ^ "[" ^ Int.toString line ^ "] " ^msg^ "\n")
	    val (ans,_) = CParser.parse (15, lexer, printError, ())
	in
            ans
	end
end



(*
structure Pi :
sig val compile : string -> DataTypes.Pi
end =
struct
exception PiError;
fun compile (fileName) =
    let val inStream =  TextIO.openIn fileName;
        val grab : int -> string = fn n => if TextIO.endOfStream inStream then ""
                                           else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn (msg,line,col) =>
                                                        print (fileName^"["^Int.toString line^":" ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = PiParser.parse (0, (PiParser.makeLexer grab fileName),printError, fileName)
        handle PiParser.ParseError => raise PiError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in tree
    end
end;
*)
