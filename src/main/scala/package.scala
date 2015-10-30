package object slisp {
	type MutMap[A, B] = scala.collection.mutable.Map[A, B]
	def MutMap[A, B](pairs: (A, B)*) = scala.collection.mutable.Map(pairs: _*)

	object SLispImplicits {
		import scala.language.implicitConversions

		implicit def double2snumber(x: Double): SNumber = SNumber(x)
		implicit def snumber2double(x: SNumber): Double = x.num

		implicit def string2sstring(x: String): SString = SString(x)
		implicit def sstring2string(x: SString): String = x.string

		implicit def slist2listsval(x: List[SVal]): SList = SList(x)
		implicit def listsval2slist(x: SList): List[SVal] = x.list

		implicit def sboolean2boolean(x: Boolean): SBoolean = SBoolean(x)
		implicit def boolean2sboolean(x: SBoolean): Boolean = x.bool
	}

	object SLisp {
		def apply(input: String): Either[SVal, SError] = {
			try {
				Left(SEvaluator(SParser(input)))
			}
			catch {
				case s: SError => Right(s)
				case t: Throwable => throw t
			}
		}
	}
}
