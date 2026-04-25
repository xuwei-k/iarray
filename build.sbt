import sbtrelease._
import ReleaseStateTransformations._

val isScala3 = Def.setting(
  CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3)
)

val Scala212 = "2.12.21"

def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

val scalazV = "7.3.8"

lazy val gitTagOrHash = Def.setting {
  if (isSnapshot.value) {
    sys.process.Process("git rev-parse HEAD").lineStream_!.head
  } else {
    "v" + version.value
  }
}

val scalaVersions = Scala212 :: "2.13.18" :: "3.3.7" :: Nil

val commonSettings = Seq[SettingsDefinition](
  publishTo := (if (isSnapshot.value) None else localStaging.value),
  (Compile / unmanagedResources) += (LocalRootProject / baseDirectory).value / "LICENSE.txt",
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) =
      new RewriteRule {
        override def transform(n: Node) =
          if (f(n)) NodeSeq.Empty else n
      }
    val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
    new RuleTransformer(stripTestScope).transform(node)(0)
  },
  releaseTagName := gitTagOrHash.value,
  releaseCrossBuild := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    releaseStepCommandAndRemaining(";scalafmtSbtCheck;scalafmtCheckAll"),
    setReleaseVersion,
    commitReleaseVersion,
    updateReadmeProcess,
    tagRelease,
    releaseStepCommandAndRemaining("publishSigned"),
    releaseStepCommandAndRemaining("sonaRelease"),
    setNextVersion,
    commitNextVersion,
    updateReadmeProcess,
    pushChanges
  ),
  organization := "com.github.xuwei-k",
  startYear := Some(2014),
  description := "Immutable array wrapper. does not use ClassTag. scalaz friendly",
  (Compile / doc / scalacOptions) ++= {
    Seq(
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-source-url",
      s"https://github.com/xuwei-k/iarray/tree/${gitTagOrHash.value}€{FILE_PATH}.scala"
    )
  },
  pomExtra := <url>https://github.com/xuwei-k/iarray</url>
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
      <tag>{gitTagOrHash.value}</tag>
    </scm>,
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-language:existentials,implicitConversions"
  ),
  scalacOptions ++= {
    scalaBinaryVersion.value match {
      case "2.12" =>
        Seq(
          "-language:higherKinds"
        )
      case _ =>
        Nil
    }
  },
  scalacOptions ++= {
    if (isScala3.value) {
      Nil
    } else {
      Seq(
        "-Xsource:3",
        "-Xlint"
      )
    }
  },
).flatMap(_.settings)

val updateReadme: State => State = { state =>
  val extracted = Project.extract(state)
  val scalaV = "2.12"
  val v = extracted.get(version)
  val org = extracted.get(organization)
  val n = "iarray"
  val readme = "README.md"
  val readmeFile = file(readme)
  val newReadme = IO
    .readLines(readmeFile)
    .map { line =>
      val matchReleaseOrSnapshot = line.contains("SNAPSHOT") == v.contains("SNAPSHOT")
      if (line.startsWith("libraryDependencies") && matchReleaseOrSnapshot) {
        if (line.contains(" %%% ")) {
          s"""libraryDependencies += "${org}" %%% "${n}" % "$v""""
        } else {
          s"""libraryDependencies += "${org}" %% "${n}" % "$v""""
        }
      } else line
    }
    .mkString("", "\n", "\n")
  IO.write(readmeFile, newReadme)
  val git = new Git(extracted.get(baseDirectory))
  git.add(readme) ! state.log
  git.commit(message = "update " + readme, sign = false, signOff = false) ! state.log
  sys.process.Process("git diff HEAD^") ! state.log
  state
}

val updateReadmeProcess: ReleaseStep = updateReadme

val iarray = projectMatrix
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .defaultAxes()
  .settings(
    commonSettings,
    name := "iarray",
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core" % scalazV
    ),
    buildInfoKeys := Seq[BuildInfoKey](
      organization,
      name,
      version,
      scalaVersion,
      sbtVersion,
      licenses,
      "scalazVersion" -> scalazV
    ),
    buildInfoPackage := "iarray",
    buildInfoObject := "BuildInfoIArray",
    scalapropsCoreSettings,
    scalapropsVersion := "0.10.1",
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % "1.19.0" % "test",
      "com.github.scalaprops" %%% "scalaprops" % scalapropsVersion.value % "test",
      "com.github.scalaprops" %%% "scalaprops-scalaz" % scalapropsVersion.value % "test"
    )
  )
  .jvmPlatform(
    scalaVersions,
  )
  .jsPlatform(
    scalaVersions,
    scalacOptions ++= {
      val a = (LocalRootProject / baseDirectory).value.toURI.toString
      val g = "https://raw.githubusercontent.com/xuwei-k/iarray/" + gitTagOrHash.value
      if (isScala3.value) {
        Seq(s"-scalajs-mapSourceURI:$a->$g/")
      } else {
        Seq(s"-P:scalajs:mapSourceURI:$a->$g/")
      }
    }
  )
  .nativePlatform(
    scalaVersions,
    scalapropsNativeSettings,
  )

val root = project
  .in(file("."))
  .aggregate(
    iarray.projectRefs *
  )
  .settings(
    commonSettings,
    commands += Command.command("updateReadme")(updateReadme),
    autoScalaLibrary := false,
    TaskKey[Unit]("testSequential") := Def
      .sequential(
        iarray.projectRefs.map(_ / Test / test)
      )
      .value,
    Compile / scalaSource := baseDirectory.value / "dummy",
    Test / scalaSource := baseDirectory.value / "dummy",
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    PgpKeys.publishSigned := {},
    PgpKeys.publishLocalSigned := {}
  )
