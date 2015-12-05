val Scala211 = "2.11.7"

scalaVersion := Scala211

crossScalaVersions := Scala211 :: "2.10.6" :: Nil

fullResolvers ~= {_.filterNot(_.name == "jcenter")}

name := "iarray"

organization := "com.github.xuwei-k"

startYear := Some(2014)

description := "Immutable array wrapper. does not use ClassTag. scalaz friendly"

def gitHash: Option[String] = scala.util.Try(
  sys.process.Process("git rev-parse HEAD").lines_!.head
).toOption

scalacOptions in (Compile, doc) ++= {
  val tag = if(isSnapshot.value) gitHash.getOrElse("master") else { "v" + version.value }
  Seq(
    "-sourcepath", baseDirectory.value.getAbsolutePath,
    "-doc-source-url", s"https://github.com/xuwei-k/iarray/tree/${tag}€{FILE_PATH}.scala"
  )
}

logBuffered in Test := false

pomExtra := (
<url>https://github.com/xuwei-k/iarray</url>
<developers>
  <developer>
    <id>xuwei-k</id>
    <name>Kenji Yoshida</name>
    <url>https://github.com/xuwei-k</url>
  </developer>
</developers>
<scm>
  <url>git@github.com:xuwei-k/iarray.git</url>
  <connection>scm:git:git@github.com:xuwei-k/iarray.git</connection>
  <tag>{if(isSnapshot.value) gitHash.getOrElse("master") else { "v" + version.value }}</tag>
</scm>
)

licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  Nil
)

val scalazV = "7.1.5"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazV,
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

val valueClasses = Seq("IArray.scala", "WithIndex.scala")

unmanagedSources in Compile := {
  val a = (unmanagedSources in Compile).value
  if(scalaVersion.value startsWith "2.10") a.filterNot(f => valueClasses.contains(f.getName))
  else a
}

sourceGenerators in Compile += task{
  if(scalaVersion.value startsWith "2.10"){
    valueClasses.map{ f =>
      val lines = IO.readLines((scalaSource in Compile).value / f).map(
        _.replace("extends AnyVal", "")
      )
      val x = (sourceManaged in Compile).value / f
      IO.writeLines(x, lines)
      x
    }
  }else Nil
}

val showDoc = TaskKey[Unit]("showDoc")

showDoc in Compile <<= (doc in Compile, target in doc in Compile) map { (_, out) =>
  java.awt.Desktop.getDesktop.open(out / "index.html")
}

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq[BuildInfoKey](
  organization,
  name,
  version,
  scalaVersion,
  sbtVersion,
  scalacOptions,
  licenses,
  "scalazVersion" -> scalazV
)

buildInfoPackage := "iarray"

buildInfoObject := "BuildInfoIArray"
