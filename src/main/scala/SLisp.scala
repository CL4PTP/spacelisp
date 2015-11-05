package slisp

object SLisp {
	def apply(input: String): SEvaluator.SEvaluatorResult = SEvaluator(SParser(input))

	def runFile(input: String, opt_env: Option[SEnv] = None): SEvaluator.SEvaluatorResult = {
		var last_result = SEvaluator.SEvaluatorResult(opt_env match {
			case Some(e) => e
			case None => SEvaluator.makeTopEnvironment
		}, SBoolean(false))

		for (parsed <- SParser parseFile(input)) {
			last_result = SEvaluator(parsed, last_result.env)
		}

		last_result
	}
}
