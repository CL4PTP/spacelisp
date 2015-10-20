lazy val root = (project in file(".")).
	settings(
		name := "Hello World",
		version := "1.0",
		scalaVersion := "2.11.7",
		sbtVersion := "0.13.9",
		scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature"),

		libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
		libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
	)
