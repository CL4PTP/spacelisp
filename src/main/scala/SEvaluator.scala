package slisp

object SEvaluator {
	import scala.language.postfixOps
	import SLispImplicits._

	def valValVal(op: ((SVal, SVal) => SVal), list: SList, e: SEnv): SVal =
		list.map(_apply(e)).reduce(op)

	def valValBool(op: ((SVal, SVal) => SBoolean), list: SList, e: SEnv): SBoolean =
		list.map(_apply(e)) match {
			case s :: xs => xs.foldLeft((SBoolean(true), s)) {
				(acc, a) => (acc._1 && op(acc._2, a), a)
			}._1
			case _ => throw SDefaultError("Not enough arguments to " + op)
		}

	def doSimpleVarBind(e: SEnv, vars: List[(String, SVal)]): SEnv = vars match {
		case (name, sval) :: xs => doSimpleVarBind(e addToCurrentScope (name.## -> _apply(e)(sval)), xs)
		case Nil => e
	}

	def doVarBind(e: SEnv, list: List[SVal]): SEnv = list match {
		case x :: xs => x match {
			case SList(SSymbol(name) :: sval :: Nil) => doVarBind(e addToCurrentScope (name.## -> _apply(e)(sval)), xs)
			case _ => throw SDefaultError("Bad bind form!")
		}
		case Nil => e
	}

	def _apply(e: SEnv)(n: SVal): SVal = n match {
		case sval @ SNumber(_) => sval
		case sval @ SString(_) => sval
		case sval @ SBoolean(_) => sval
		case SSymbol(name) => e get name match {
			case Some(sval) => sval
			case None => throw SUnboundVarError("Simple evaluation failed", name)
		}

		case SList(List(SSymbol("quote"), sval)) => sval
		case SList(List(SSymbol("-"), SNumber(num))) => SNumber(-1 * num)

		case SList(SSymbol("if") :: pred :: conseq :: alt) => _apply(e)(pred) match {
			case SBoolean(false) => _apply(e)(alt)
			case _ => _apply(e)(conseq)
		}

		case SList(SSymbol("car") :: stail) => stail.map(_apply(e)) match {
			case List(SList(x :: _)) => x
			case badArgs => throw SDefaultError("Unable to `car` value " + badArgs)
		}
		case SList(SSymbol("cdr") :: stail) => stail.map(_apply(e)) match {
			case List(SList(_ :: xs)) => xs
			case badArgs => throw SDefaultError("Unable to `cdr` value " + badArgs)
		}
		case SList(SSymbol("cons") :: stail) => stail.map(_apply(e)) match {
			case List(x, SList(xs)) => SList(x :: xs)
			case List(x, y) => SList(List(x, y)) // TODO: decide if this is OK
			case badArgs => throw SDefaultError("Unable to `cons` value " + badArgs)
		}

		case SList(SSymbol("bind") :: SList(val_exprs) :: bodies) => {
			val nenv = doVarBind(e newScope, val_exprs)

			bodies map (_apply(nenv)) lastOption match {
				case Some(s) => s
				case None => throw SDefaultError("Bind has to have a body")
			}
		}

		case SList(SSymbol("lambda") :: SList(params) :: expressions) =>
			SFnc(params map {
				case SSymbol(name) => name
				case badArg => throw SDefaultError("Expected symbol, found " + badArg.pprint)
			}, expressions, e)

		case SList(SSymbol("display") :: to_display) => {
			to_display map _apply(e) foreach (x => println(x.pprint))

			to_display last //BUG: should be to_display map _apply(e) last, but that's ugly
		}

		case SList(symbol :: stail) => symbol match {
			case SSymbol("+") => valValVal(_ + _, stail, e)
			case SSymbol("-") => valValVal(_ - _, stail, e)
			case SSymbol("*") => valValVal(_ * _, stail, e)
			case SSymbol("/") => valValVal(_ / _, stail, e)
			case SSymbol("%") => valValVal(_ % _, stail, e)

			case SSymbol("==") => valValBool(_ == _, stail, e)
			case SSymbol("!=") => valValBool(_ != _, stail, e)

			case SSymbol("<") => valValBool(_ < _, stail, e)
			case SSymbol("<=") => valValBool(_ <= _, stail, e)
			case SSymbol(">") => valValBool(_ > _, stail, e)
			case SSymbol(">=") => valValBool(_ >= _, stail, e)

			case SSymbol("|") => valValVal(_ | _, stail, e)
			case SSymbol("&") => valValVal(_ & _, stail, e)
			case SSymbol("||") => valValBool(_ || _, stail, e)
			case SSymbol("&&") => valValBool(_ && _, stail, e)
			case SSymbol("weak==") => valValBool({
				(a, b) => (a weak_== b) || (b weak_== a)
			}, stail, e)

			case unknown => _apply(e)(unknown) match {
				case SFnc(params, body, closure) => {
					if (params.length != stail.length) throw SNumArgsError(params length, stail)

					body map _apply(doSimpleVarBind(closure.newScope, params zip stail)) last
				}
				case _ => throw SNotAFunctionError("Unrecognized", unknown.pprint)
			}
		}
		case _ => throw SDefaultError("Unable to process " + n)
	}

	def apply(node: SVal): SVal = _apply(SEnv(List(Map())))(node)
}
