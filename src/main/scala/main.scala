import scala.util.parsing.combinator._

object SLispParser extends JavaTokenParsers {
	val number = floatingPointNumber
	val symbol = "+" | "-" | "*" | "/"
	val sexpr = "(" ~> expr.* <~ ")"
	val expr: Parser[Any] = number | symbol | sexpr
	val program = expr.*

	// ---

	def apply(input: String) = parseAll(program, input) match {
 		case Success(result, _) => result
 		case failure : NoSuccess => scala.sys.error(failure.msg)
 	}
}

object Main extends App {
	if (args.length > 0)
	{
		Console.println(SLispParser(args mkString ""))
	}
	else
	{
		Console.println(SLispParser("- 1 2 3 (+ 1 1)"))
	}
}
