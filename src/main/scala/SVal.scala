package slisp

object Helpers {
	def isStringDouble(str: String): Boolean = {
		try { str.toDouble; true } catch { case _: NumberFormatException => false }
	}

	def isStringBoolean(str: String): Boolean = {
		try { str.toBoolean; true } catch { case _: NumberFormatException => false }
	}
}

sealed trait SVal {
	def weak_==(o: SVal): Boolean = this == o

	def pprint: String = this.toString

	def +(o: SVal): SVal = (this, o) match {
		case (SString(a), SString(b)) => SString(a + b)
		case (SNumber(a), SNumber(b)) => SNumber(a + b)
		case _ => throw new IllegalArgumentException("Can't add " + o + " and " + this)
		// BUG: change the above to case _ => throw STypeMismatchError("String|Number", o)
	}

	def -(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a - b)
		case _ => throw new IllegalArgumentException("Can't subtract " + o + " and " + this)
	}

	def *(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a * b)
		case _ => throw new IllegalArgumentException("Can't multiply " + o + " and " + this)
	}

	def /(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a / b)
		case _ => throw new IllegalArgumentException("Can't divide " + o + " and " + this)
	}

	def %(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a % b)
		case _ => throw new IllegalArgumentException("Can't modulo " + o + " and " + this)
	}

	// ---

	def <(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a < b)
		case (SString(a), SString(b)) => SBoolean(a < b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a < b)
		case _ => throw new IllegalArgumentException("Can't lt " + o + " and " + this)
	}

	def <=(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a <= b)
		case (SString(a), SString(b)) => SBoolean(a <= b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a <= b)
		case _ => throw new IllegalArgumentException("Can't lte " + o + " and " + this)
	}

	def >(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a > b)
		case (SString(a), SString(b)) => SBoolean(a > b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a > b)
		case _ => throw new IllegalArgumentException("Can't gt " + o + " and " + this)
	}

	def >=(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a >= b)
		case (SString(a), SString(b)) => SBoolean(a >= b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a >= b)
		case _ => throw new IllegalArgumentException("Can't gte " + o + " and " + this)
	}

	// ---

	def |(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a.toLong | b.toLong)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a | b)
		case _ => throw new IllegalArgumentException("Can't binary or " + o + " and " + this)
	}

	def &(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a.toLong & b.toLong)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a & b)
		case _ => throw new IllegalArgumentException("Can't binary and " + o + " and " + this)
	}

	// def unary_~: SVal = this match {
	// 	case SNumber(a) => SNumber(~a.toLong)
	// 	case SBoolean(a) => SBoolean(~a)
	// 	case _ => throw new IllegalArgumentException("Can't subtract " + o + " and " + this)
	// }

	// ---

	def ||(o: SVal): SBoolean = (this, o) match {
		case (SBoolean(a), SBoolean(b)) => SBoolean(a || b)
		case _ => throw new IllegalArgumentException("Can't logical or a " + o + " and a " + this)
	}

	def &&(o: SVal): SBoolean = (this, o) match {
		case (SBoolean(a), SBoolean(b)) => SBoolean(a && b)
		case _ => throw new IllegalArgumentException("Can't logical subtract a " + o + " and a " + this)
	}

	// def unary_!: SBoolean = this match {
	// 	case SBoolean(a) => SBoolean(!a)
	// 	case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	// }
}

case class SSymbol (name: String) extends SVal {
	import Helpers._

	override def weak_==(o: SVal): Boolean = o match {
		case SSymbol(_) => this == o
		case SBoolean(obool) => isStringBoolean(name) && name.toBoolean == obool
		case SNumber(onum) => isStringDouble(name) && name.toDouble == onum
		case SString(ostring) => name == ostring
		case _ => false
	}

	override def pprint: String = name
}
case class SBoolean (bool: Boolean) extends SVal {
	override def weak_==(o: SVal): Boolean = o match {
		case SSymbol(_) => o weak_== this
		case SBoolean(_) => this == o
		case SString(ostring) => SSymbol(ostring) weak_== this
		case _ => false
	}

	override def pprint: String = bool.toString
}
case class SNumber (num: Double) extends SVal {
	override def weak_==(o: SVal): Boolean = o match {
		case SSymbol(_) => o weak_== this
		case SNumber(_) => this == o
		case SString(ostring) => SSymbol(ostring) weak_== this
		case _ => false
	}

	override def pprint: String = num.toString
}
case class SString (string: String) extends SVal {
	override def weak_==(o: SVal): Boolean = SSymbol(string) weak_== o

	override def pprint: String = string
}
case class SList (list: List[SVal]) extends SVal {
	override def pprint: String = list map(_.pprint) mkString("(", ", ", ")")
}
case class SPrimitiveFnc (fnc: List[SVal] => SVal) extends SVal {
	override def pprint: String = "<primitive>"
}
case class SFnc (params: List[String], body: List[SVal], closure: SEnv) extends SVal {
	override def pprint: String = "(lambda (" + (params mkString " ") + ") ...)"
}
