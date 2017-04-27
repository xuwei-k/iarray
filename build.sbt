val Scala211 = "2.11.11"

scalaVersion := Scala211

crossScalaVersions := Scala211 :: "2.10.6" :: "2.12.2" :: Nil

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

val unusedWarnings = (
  "-Ywarn-unused-import" ::
  Nil
)

scalacOptions ++= PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)){
  case Some((2, v)) if v >= 11 => unusedWarnings
}.toList.flatten

Seq(Compile, Test).flatMap(c =>
  scalacOptions in (c, console) ~= {_.filterNot(unusedWarnings.toSet)}
)

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  Nil
)

val scalazV = "7.2.11"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazV,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test" // use in doctest
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

enablePlugins(BuildInfoPlugin)

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
