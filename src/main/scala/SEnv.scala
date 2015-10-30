package slisp

case class SEnv (val scopes: List[MutMap[String, SVal]]) {
	def get(name: String): Option[SVal] = scopes find (_ contains name) map (_(name))

	def newScope: SEnv = SEnv(MutMap[String, SVal]() :: scopes)

	def addToCurrentScope(pair: (String, SVal)): SEnv = {
		scopes.head += pair
		this
	}
}