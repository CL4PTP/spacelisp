package slisp

case class SEnv (val scopes: List[Map[Int, SVal]]) {
	def get (name: String): Option[SVal] = get(name.##)
	def get (name: Int): Option[SVal] = scopes find (_ contains name) map (_(name))

	def newScope: SEnv = SEnv((Map(): Map[Int, SVal]) :: scopes)

	def addToCurrentScope(pair: (Int, SVal)): SEnv = scopes match {
		case x :: xs => SEnv((x + pair) :: xs)
		case _ => throw SDefaultError("Unable to add variable to scope (THIS IS BAD)")
	}
}