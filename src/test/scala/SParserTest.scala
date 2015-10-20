package slisp

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
	test("Simple empty list") {
		assertResult(SList(List())) {
			SParser("()")
		}
	}
}
