package slisp

object SEvaluator {
	import SLispImplicits._

	def valValVal(op: ((SVal, SVal) => SVal), list: SList): SVal =
		list.map(apply).reduce(op)

	def valValBool(op: ((SVal, SVal) => SBoolean), list: SList): SBoolean =
		list.map(apply) match {
			case s :: xs => xs.foldLeft((SBoolean(true), s)) {
				(acc, a) => (acc._1 && op(acc._2, a), a)
			}._1
			case _ => throw new IllegalArgumentException("Not enough arguments to " + op)
		}

	def apply(node: SVal): SVal = node match {
		case sval @ SNumber(_) => sval
		case sval @ SString(_) => sval
		case sval @ SSymbol(_) => sval
		case sval @ SBoolean(_) => sval

		case SList(SSymbol("quote") :: sval) => SList(sval)
		case SList(SSymbol("-") :: List(SNumber(num))) => SNumber(-1 * num)

		case SList(List(SSymbol("if"), pred, conseq, alt)) => apply(pred) match {
			case SBoolean(false) => apply(alt)
			case _ => apply(conseq)
		}

		case SList(SSymbol("car") :: stail) => apply(stail) match {
			case List(SList(x :: xs)) => x
			case badArgs => throw new IllegalArgumentException("Unable to `car` value " + badArgs)
		}

		case SList((symbol: SSymbol) :: stail) => symbol match {
			case SSymbol("+") => valValVal(_ + _, stail)
			case SSymbol("-") => valValVal(_ - _, stail)
			case SSymbol("*") => valValVal(_ * _, stail)
			case SSymbol("/") => valValVal(_ / _, stail)
			case SSymbol("%") => valValVal(_ % _, stail)

			case SSymbol("==") => valValBool(_ == _, stail)
			case SSymbol("!=") => valValBool(_ != _, stail)

			case SSymbol("<") => valValBool(_ < _, stail)
			case SSymbol("<=") => valValBool(_ <= _, stail)
			case SSymbol(">") => valValBool(_ > _, stail)
			case SSymbol(">=") => valValBool(_ >= _, stail)

			case SSymbol("|") => valValVal(_ | _, stail)
			case SSymbol("&") => valValVal(_ & _, stail)
			case SSymbol("||") => valValBool(_ || _, stail)
			case SSymbol("&&") => valValBool(_ && _, stail)

			case _ => throw new IllegalArgumentException("No such function: " + symbol.name)
		}
		case _ => throw new IllegalArgumentException("Illegal action: " + node)
	}
}
