testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false

val v = "0.1.4"
libraryDependencies += "com.github.xuwei-k" %% "scalaprops" % v % "test"
libraryDependencies += "com.github.xuwei-k" %% "scalaprops-scalazlaws" % v % "test"
