package slisp

import scala.util.parsing.combinator._

object SParser extends JavaTokenParsers {
	val number: Parser[SNumber] = floatingPointNumber ^^ {
		x => SNumber(x.toDouble)
	}
	val bool: Parser[SBoolean] = ("true" | "false") ^^ {
		x => SBoolean(x.toBoolean)
	}
	val string: Parser[SString] = stringLiteral ^^ {
		x => SString(x.init.tail)
	}
	val symbol: Parser[SSymbol] = """[A-Za-z0-9!#$%&|*+\-/:<=>?@\^_~]+""".r ^^ {
		x => SSymbol(x)
	}
	val list: Parser[SList] = "(" ~> expr.* <~ ")" ^^ {
		x => SList(x)
	}
	val quoted: Parser[SList] = "'" ~> expr ^^ {
		x => SList(List(SSymbol("quote"), x))
	}
	val expr: Parser[SVal] = number | string | bool | symbol | list | quoted
	val file: Parser[List[SVal]] = list.*

	// ---

	def apply(input: String) = parseAll(list, input) match {
		case Success(result, _) => result
		case unknownFailure: NoSuccess => throw SParseError(unknownFailure)
	}

	def parseFile(input: String) = parseAll(file, input) match {
		case Success(result, _) => result
		case unknownFailure: NoSuccess => throw SParseError(unknownFailure)
	}
}
