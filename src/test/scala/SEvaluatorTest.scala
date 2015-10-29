package slisp

import org.scalatest.FunSuite

class EvaluatorTest extends FunSuite {
	import SLispImplicits._

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

	test("Weak equality") {
		testTrue("""(weak== 1 '1)""")
		testTrue("""(weak== 1 "1")""")
		testTrue("""(weak== true 'true)""")
		testTrue("""(weak== true "true")""")

		testTrue("""(weak== "yes" 'yes)""")
		testTrue("""(weak== 'yes "yes")""")

		testTrue("""(weak== 1.0 1)""")
		testTrue("""(weak== 1.0 0.99999999999999999999999999999999999)""")

		testTrue("""(weak== 1 1.0 1 1.0 '1 "1" '1)""")
		testTrue("""(weak== '1 "1" 1)""")
	}

	test("Simple string comparison") {
		testTrue("""(< "a" "b")""")
		testTrue("""(< "hello world" "hi world")""")
		testTrue("""(< "10" "2")""")
	}

	test("Simple conditionals") {
		testString("""(if (< 1 2) "hi" "IMPOSSIBLE!")""") ("hi")
		testString("""(if (> 1 2) "hi" "IMPOSSIBLE!")""") ("IMPOSSIBLE!")
	}

	test("Simple list primitives") {
		assertResult(SNumber(1)) (SLisp("(car '(1 2 3))"))
		assertResult(SList(List(2, 3))) (SLisp("(cdr '(1 2 3))"))
	}

	test("List `cons`s") {
		assertResult(SList(List(1, 2, 3))) (SLisp("(cons 1 '(2 3))"))
		assertResult(SList(List(1))) (SLisp("(cons 1 '())"))

		assertResult(SList(List(1, 2))) (SLisp("(cons 1 2)"))
	}

	test("Binds") {
		assertResult(SNumber(1)) (SLisp("(bind ((hi 1) (ho (+ hi 1))) (+ hi ho) (- ho hi))"))

		// assertResult(SNumber(1)) (SLisp("((bind ((hi 1) (ho (+ hi 1))) (+ hi ho) (- ho hi))"))
	}
}
