package slisp

sealed trait SVal {
	import SLispImplicits._

	def +(o: SVal): SVal = (this, o) match {
		case (SString(a), SString(b)) => SString(a + b)
		case (SNumber(a), SNumber(b)) => SNumber(a + b)
		case _ => throw new IllegalArgumentException("Can't add a " + o + " and a " + this)
	}

	def -(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a - b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def *(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a * b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def /(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a / b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def %(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a % b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	// ---

	def <(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a < b)
		case (SString(a), SString(b)) => SBoolean(a < b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a < b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def <=(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a <= b)
		case (SString(a), SString(b)) => SBoolean(a <= b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a <= b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def >(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a > b)
		case (SString(a), SString(b)) => SBoolean(a > b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a > b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def >=(o: SVal): SBoolean = (this, o) match {
		case (SNumber(a), SNumber(b)) => SBoolean(a >= b)
		case (SString(a), SString(b)) => SBoolean(a >= b)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a >= b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	// ---

	def |(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a.toLong | b.toLong)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a | b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def &(o: SVal): SVal = (this, o) match {
		case (SNumber(a), SNumber(b)) => SNumber(a.toLong & b.toLong)
		case (SBoolean(a), SBoolean(b)) => SBoolean(a & b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	// def unary_~: SVal = this match {
	// 	case SNumber(a) => SNumber(~a.toLong)
	// 	case SBoolean(a) => SBoolean(~a)
	// 	case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	// }

	// ---

	def ||(o: SVal): SBoolean = (this, o) match {
		case (SBoolean(a), SBoolean(b)) => SBoolean(a || b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	def &&(o: SVal): SBoolean = (this, o) match {
		case (SBoolean(a), SBoolean(b)) => SBoolean(a && b)
		case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	}

	// def unary_!: SBoolean = this match {
	// 	case SBoolean(a) => SBoolean(!a)
	// 	case _ => throw new IllegalArgumentException("Can't subtract a " + o + " and a " + this)
	// }
}

case class SSymbol (name: String) extends SVal {
	override def toString: String = name
}
case class SNumber (num: Double) extends SVal {
	override def toString: String = num.toString
}
case class SString (string: String) extends SVal {
	override def toString: String = '"' + string + '"'
}
case class SBoolean (bool: Boolean) extends SVal {
	override def toString: String = bool.toString
}
case class SList (list: List[SVal]) extends SVal {
	override def toString: String = list.mkString(" ", "(", ")")
}
