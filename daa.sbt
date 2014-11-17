name := "Daa"

version := "0.0.1"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

fork in run := true

javaOptions in run ++= Seq("-Xmx4G","-Xms4G", "-Xss64M")
