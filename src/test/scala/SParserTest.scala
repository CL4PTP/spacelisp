package slisp

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
	def escapeOuterList(expected: SVal) (source: String) = 
		assertResult(SList(List(expected))) (SParser(source))

	test("Empty list") {
		assertResult(SList(List())) (SParser("()"))
	}

	test("String without quotes") {
		escapeOuterList(SString("hi")) ("(\"hi\")")
	}
}
