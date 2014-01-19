scalaVersion := "2.11.0-M7"

crossScalaVersions := List("2.11.0-M7")

incOptions := incOptions.value.withNameHashing(true)

name := "iarray"

organization := "com.github.xuwei-k"

startYear := Some(2014)

scmInfo := Some(ScmInfo(
  url("https://github.com/xuwei-k/iarray"),
  "scm:git:git@github.com:xuwei-k/iarray.git"
))

description := "Immutable array wrapper. does not use ClassTag. scalaz friendly"

scalacOptions in (Compile, doc) ++= {
  val v = version.value
  val tag = if(v endsWith "SNAPSHOT") "master" else { "v" + v }
  Seq(
    "-sourcepath", baseDirectory.value.getAbsolutePath,
    "-doc-source-url", s"https://github.com/xuwei-k/iarray/tree/${tag}€{FILE_PATH}.scala"
  )
}

logBuffered in Test := false

pomExtra := (
<developers>
  <developer>
    <id>xuwei-k</id>
    <name>Kenji Yoshida</name>
    <url>https://github.com/xuwei-k</url>
  </developer>
</developers>
)

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalacOptions ++= Seq("-deprecation", "-Xlint", "-unchecked", "-language:_")

val scalazV = "7.1.0-M4"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazV,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazV % "test"
)

val specLiteURL = "https://raw.github.com/scalaz/scalaz/v7.1.0-M4/tests/src/test/scala/scalaz/SpecLite.scala"
val specLite = SettingKey[List[String]]("specLite")

specLite := {
  println(s"downloading from ${specLiteURL}")
  val lines = IO.readLinesURL(url(specLiteURL))
  println("download finished")
  lines
}

def specLiteFile(dir: File, contents: List[String]): File = {
  val file = dir / "SpecLite.scala"
  IO.writeLines(file, contents)
  file
}

sourceGenerators in Test += task{
  Seq(specLiteFile((sourceManaged in Test).value, specLite.value))
}

val showDoc = TaskKey[Unit]("showDoc")

showDoc in Compile <<= (doc in Compile, target in doc in Compile) map { (_, out) =>
  java.awt.Desktop.getDesktop.open(out / "index.html")
}
