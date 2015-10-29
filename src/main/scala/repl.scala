package slisp

import java.io.PrintWriter
import jline.console.ConsoleReader

object Main {
	def streamLines(reader: ConsoleReader): Stream[String] = {
		val line = reader.readLine
		if (line == null) Stream.empty
		else Stream.cons(line, streamLines(reader))
	}

	def main(args: Array[String]): Unit = {
		try {
			val reader = new ConsoleReader()
			var line = new String()
			val out = new PrintWriter(reader.getOutput())

			reader setPrompt "s> "

			for (line <- streamLines(reader)) {
				line match {
					case "quit" | "exit" => return
					case "cls" => reader.clearScreen
					case lisp => {
						out println (SLisp(lisp) match {
							case Left(sval) => sval.pprint
							case Right(error) => error match {
								case SParseError(error) => "Parse error: " + error
								case SNumArgsError(exp, found) => "Expected " + exp + " arguments, found " + found.length + " " + SList(found).pprint + ""
								case STypeMismatchError(exp, found) => "Expected type " + exp + ", found " + found
								case SNotAFunctionError(msg, fnc) => fnc + " is not a function (" + msg + ")"
								case SUnboundVarError(msg, vname) => vname + " is not a variable (" + msg + ")"
								case SDefaultError(msg) => "Error: " + msg
							}
						})
						out.flush()
					}
				}
			}
		} catch {
			case t: Throwable => t.printStackTrace
		}
	}
}
