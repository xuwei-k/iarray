testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

val v = "0.1.5"
libraryDependencies += "com.github.scalaprops" %% "scalaprops" % v % "test"
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalazlaws" % v % "test"
