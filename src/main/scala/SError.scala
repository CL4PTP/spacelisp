package slisp

import scala.util.parsing.combinator.Parsers

sealed trait SError extends RuntimeException
case class SParseError (error: Parsers#NoSuccess) extends SError
case class SNumArgsError (expected: Int, found: List[SVal]) extends SError
case class STypeMismatchError (expected: String, found: SVal) extends SError
case class SNotAFunctionError (msg: String, fnc: String) extends SError
case class SUnboundVarError (msg: String, vname: String) extends SError
case class SDefaultError (msg: String) extends SError
