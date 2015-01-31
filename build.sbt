name := "scalascrap"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.5" % "test"

libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.1.0-RC2")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.1.0"
