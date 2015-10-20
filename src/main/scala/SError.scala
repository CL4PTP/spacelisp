package slisp

sealed trait SError
case class SNumArgs (expected: Int, found: List[SVal]) extends SError
case class STypeMismatch (expected: String, found: SVal) extends SError
case class SParse (error: Error) extends SError
case class SBadSpecialForm (msg: String, form: SVal) extends SError
case class SNotAFunction (msg: String, fnc: String) extends SError
case class SUnboundVar (msg: String, vname: String) extends SError
case class SDefaultError (msg: String) extends SError
