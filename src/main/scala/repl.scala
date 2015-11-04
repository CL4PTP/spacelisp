package slisp

import java.io.PrintWriter
import jline.console.ConsoleReader
import org.apache.commons.cli.{CommandLine, DefaultParser, Options}

object Main {
	def streamLines(reader: ConsoleReader): Stream[String] = {
		val line = reader.readLine
		if (line == null) Stream.empty
		else Stream.cons(line, streamLines(reader))
	}

	def main(args: Array[String]): Unit = {
		val options = new Options();
		options.addOption("r", "run", true, "run a file instead of opening a prompt");

		val parser = new DefaultParser();
		val cmd = parser.parse(options, args);

		if (cmd hasOption "r") {
			val source = scala.io.Source.fromFile(cmd getOptionValue "r")
			try {
				SLisp runFile(source.getLines mkString "\n")
			}
			catch {
				case serr: SError => printSError(serr)
			}
			finally {
				source.close()
			}
		} else {
			try {
				val reader = new ConsoleReader()
				var line = new String()
				val out = new PrintWriter(reader.getOutput())

				reader setPrompt "s> "

				var last_result = SEvaluator.SEvaluatorResult(SEvaluator.makeTopEnvironment, SBoolean(false))

				for (line <- streamLines(reader)) {
					line match {
						case "quit" | "exit" => return
						case "cls" => reader.clearScreen
						case lisp => {
							last_result = SLisp runFile(lisp, Some(last_result.env))

							out.println(last_result.result.pprint)
							out.flush
						}
					}
				}
			} catch {
				case serr: SError => printSError(serr)
			}
		}
	}

	def printSError(serr: SError) = println(serr match {
		case SParseError(error) => "Parse error: " + error
		case SNumArgsError(exp, found) => "Expected " + exp + " arguments, found " + found.length + " " + SList(found).pprint + ""
		case STypeMismatchError(exp, found) => "Expected type " + exp + ", found " + found
		case SNotAFunctionError(msg, fnc) => fnc + " is not a function (" + msg + ")"
		case SUnboundVarError(msg, vname) => vname + " is not a variable (" + msg + ")"
		case SDefaultError(msg) => "Error: " + msg
	})
}
