package slisp

import org.scalatest.FunSuite

class EvaluatorTest extends FunSuite {
	import SLispImplicits._

	def testNum(expected: Double)(expr: String) = {
		assertResult(Left(SNumber(expected)))(SLisp(expr))
	}

	def testString(expected: String)(expr: String) = {
		assertResult(Left(SString(expected)))(SLisp(expr))
	}

	def testTrue(expr: String) = {
		assertResult(Left(SBoolean(true)))(SLisp(expr))
	}

	def testArbitrary(expected: SVal)(value: Either[SVal, SError]) = {
		assertResult(Left(expected))(value)
	}
	
	// ---

	test("Simple addition") {
		testNum(1) ("(+ 1)")
		testNum(1 + 2 + 3) ("(+ 1 2 3)")
		testNum(1 + -2 + 3) ("(+ 1 -2 3)")
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
		testString("hi") ("""(if (< 1 2) "hi" "IMPOSSIBLE!")""")
		testString("IMPOSSIBLE!") ("""(if (> 1 2) "hi" "IMPOSSIBLE!")""")
	}

	test("Simple list primitives") {
		testArbitrary(SNumber(1)) (SLisp("(car '(1 2 3))"))
		testArbitrary(SList(List(2, 3))) (SLisp("(cdr '(1 2 3))"))
	}

	test("List `cons`s") {
		testArbitrary(SList(List(1, 2, 3))) (SLisp("(cons 1 '(2 3))"))
		testArbitrary(SList(List(1))) (SLisp("(cons 1 '())"))

		testArbitrary(SList(List(1, 2))) (SLisp("(cons 1 2)"))
	}

	test("Binds") {
		testArbitrary(SNumber(1)) (SLisp("(bind ((hi 1) (ho (+ hi 1))) (+ hi ho) (- ho hi))"))
	}

	test("Recursion") {
		testArbitrary(SNumber(6)) (SLisp("""
			(bind
				(
					(fac (lambda (s)

						(if (<= s 1)
							1
							(* s (fac (- s 1)))
						)
					))
				)

			(fac 3)
			
			)
		"""))
	}
}
