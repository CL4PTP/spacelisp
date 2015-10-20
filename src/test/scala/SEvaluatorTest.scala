package slisp

import org.scalatest.FunSuite

class EvaluatorTest extends FunSuite {
	def testNum(expr: String)(expected: Double) = {
		assertResult(SNumber(expected))(SLisp(expr))
	}

	def testString(expr: String)(expected: String) = {
		assertResult(SString(expected))(SLisp(expr))
	}

	def testTrue(expr: String) = {
		assertResult(SBoolean(true))(SLisp(expr))
	}
	
	// ---

	test("Simple addition") {
		testNum("(+ 1)") (1)
		testNum("(+ 1 2 3)") (1 + 2 + 3)
		testNum("(+ 1 -2 3)") (1 + -2 + 3)
	}

	test("Simple string comparison") {
		testTrue("""(< "a" "b")""")
		testTrue("""(< "hello world" "hi world")""")
		testTrue("""(< "10" "2")""")
	}

	test("Simple conditionals") {
		testString("""(if (< 1 2) "hi" "IMPOSSIBLE!")""") ("\"hi\"")
		testString("""(if (> 1 2) "hi" "IMPOSSIBLE!")""") ("\"IMPOSSIBLE!\"")
	}

	test("Simple list primitives") {
		assertResult(SNumber(1)) (SLisp("(car '(1 2 3))"))
	}
}
