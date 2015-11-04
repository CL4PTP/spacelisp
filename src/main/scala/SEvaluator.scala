package slisp

object SEvaluator {
	import scala.language.postfixOps
	import SLispImplicits._

	case class SEvaluatorResult(env: SEnv, result: SVal)

	def valValVal(op: (SVal, SVal) => SVal): List[SVal] => SVal =
		_ reduce op

	def valValBool(op: ((SVal, SVal) => SBoolean)): List[SVal] => SVal =
		_ match {
			case s :: xs => xs.foldLeft((SBoolean(true), s)) {
				(acc, a) => (acc._1 && op(acc._2, a), a)
			}._1
			case _ => throw SDefaultError("Not enough arguments to " + op)
		}

	def doVarBind(e: SEnv)(vars: List[(String, SVal)]): SEnv = vars match {
		case (name, sval) :: xs => doVarBind(e addToCurrentScope (name -> _apply(e)(sval)))(xs)
		case Nil => e
	}

	def _apply(e: SEnv)(n: SVal): SVal = n match {
		case sval @ SNumber(_) => sval
		case sval @ SString(_) => sval
		case sval @ SBoolean(_) => sval
		case SSymbol(name) => e get name match {
			case Some(sval) => sval
			case None => throw SUnboundVarError("simple evaluation failed", name)
		}

		case SList(List(SSymbol("quote"), sval)) => sval

		case SList(SSymbol("if") :: pred :: conseq :: alt :: Nil) => _apply(e)(pred) match {
			case SBoolean(false) => _apply(e)(alt)
			case _ => _apply(e)(conseq)
		}

		case SList(SSymbol("bind") :: SList(val_exprs) :: bodies) => {
			val nenv = doVarBind(e newScope)(val_exprs map (_ match {
				case SList(SSymbol(name) :: bind :: Nil) => (name -> bind)
				case _ => throw SDefaultError("Bad bind form")
			}))

			bodies map (_apply(nenv)) lastOption match {
				case Some(s) => s
				case None => throw SDefaultError("Bind has to have a body")
			}
		}

		case SList(SSymbol("scope-bind") :: val_exprs) => {
			doVarBind(e)(val_exprs map (_ match {
				case SList(SSymbol(name) :: bind :: Nil) => (name -> bind)
				case _ => throw SDefaultError("Bad bind form")
			}))

			SBoolean(false)
		}

		case SList(SSymbol("lambda") :: SList(params) :: expressions) =>
			SFnc(params map {
				case SSymbol(name) => name
				case badArg => throw SDefaultError("Expected symbol, found " + badArg.pprint)
			}, expressions, e newScope)

		case SList(symbol :: stail) => _apply(e)(symbol) match {
			case SFnc(params, body, closure) => {
				if (params.length != stail.length) throw SNumArgsError(params length, stail)

				// TODO: find out why replace `closure` with `closure newScope` breaks recursion
				val function_scope = doVarBind(closure)(params zip stail)

				body map _apply(function_scope) last
			}
			case SPrimitiveFnc (fnc) => fnc(stail map _apply(e))
			case _ => throw SNotAFunctionError("Unrecognized", symbol.pprint)
		}
		case _ => throw SDefaultError("Unable to process " + n)
	}

	def makeTopEnvironment: SEnv = SEnv(List(MutMap(
			"+" -> valValVal(_ + _),
			"-" -> SPrimitiveFnc(x =>
				if (x.length == 1) -1 * x(0)
				else valValVal(_ - _)(x)
			),
			"*" -> valValVal(_ * _),
			"/" -> valValVal(_ / _),
			"%" -> valValVal(_ % _),

			"==" -> valValBool(_ == _),
			"!=" -> valValBool(_ != _),

			"<" -> valValBool(_ < _),
			"<=" -> valValBool(_ <= _),
			">" -> valValBool(_ > _),
			">=" -> valValBool(_ >= _),

			"|" -> valValVal(_ | _),
			"&" -> valValVal(_ & _),
			"||" -> valValBool(_ || _),
			"&&" -> valValBool(_ && _),
			"weak==" -> valValBool({
				(a, b) => (a weak_== b) || (b weak_== a)
			}),

			"display" -> SPrimitiveFnc(x => {
				x.foreach(y => println(y.pprint))
				x last
			}),

			"car" -> SPrimitiveFnc(_ match {
				case List(SList(x :: _)) => x
				case badArgs => throw SDefaultError("Unable to `car` value " + badArgs)
			}),
			"cdr" -> SPrimitiveFnc(_ match {
				case List(SList(_ :: xs)) => SList(xs)
				case badArgs => throw SDefaultError("Unable to `cdr` value " + badArgs)
			}),
			"cons" -> SPrimitiveFnc(_ match {
				case List(x, SList(xs)) => SList(x :: xs)
				case List(x, y) => SList(List(x, y)) // TODO: decide if this is OK
				case badArgs => throw SDefaultError("Unable to `cons` value " + badArgs)
			})
		)))

	def apply(node: SVal, env: SEnv = makeTopEnvironment): SEvaluatorResult = {
		SEvaluatorResult(env, _apply(env)(node))
	}
}
