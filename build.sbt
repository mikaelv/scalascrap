name := "scalascrap"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.0.0")
