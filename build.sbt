name := "tifmo"
version := "1.0"
scalaVersion := "2.12.3"

unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "ext" / "scala"

unmanagedJars in Compile ++=
  ((baseDirectory.value / "lib") * "*.jar").classpath

unmanagedJars in Compile ++=
  ((baseDirectory.value / "lib" / "en") * "*.jar").classpath

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-simple" % "1.7.25",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" classifier "models",
)
