name := "Meruem"

version := "1.1.2"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "com.typesafe" % "config" % "1.3.0",
  "commons-lang" % "commons-lang" % "2.6",
  "com.jsuereth" %% "scala-arm" % "1.4"
)

assemblyJarName in assembly := "meruem.jar"
    
