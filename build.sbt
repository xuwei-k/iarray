import com.typesafe.sbt.pgp.PgpKeys
import sbtcrossproject.crossProject
import sbtrelease._
import ReleaseStateTransformations._

def releaseStepCross[A](key: TaskKey[A]) = ReleaseStep(
  action = { state =>
    val extracted = Project extract state
    extracted.runAggregated(key in Global in extracted.get(thisProjectRef), state)
  },
  enableCrossBuild = true
)

val CustomCrossType = new sbtcrossproject.CrossType {
  override def projectDir(crossBase: File, projectType: String) =
    crossBase / projectType

  override def projectDir(crossBase: File, projectType: sbtcrossproject.Platform) = {
    val dir = projectType match {
      case JVMPlatform => "jvm"
      case JSPlatform => "js"
      case NativePlatform => "native"
    }
    crossBase / dir
  }

  def shared(projectBase: File, conf: String) =
    projectBase.getParentFile / "src" / conf / "scala"

  override def sharedSrcDir(projectBase: File, conf: String) =
    Some(shared(projectBase, conf))
}

val Scala211 = "2.11.12"

def gitHash(): String = sys.process.Process("git rev-parse HEAD").lineStream_!.head

val scalazV = "7.2.27"

lazy val gitTagOrHash = Def.setting {
  if (isSnapshot.value) {
    sys.process.Process("git rev-parse HEAD").lineStream_!.head
  } else {
    "v" + version.value
  }
}

val commonSettings = Seq[SettingsDefinition](
  publishTo := Some(
    if (isSnapshot.value)
      Opts.resolver.sonatypeSnapshots
    else
      Opts.resolver.sonatypeStaging
  ),
  unmanagedResources in Compile += (baseDirectory in LocalRootProject).value / "LICENSE.txt",
  credentials in Global ++= PartialFunction
    .condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASS")) {
      case (Some(user), Some(pass)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    }
    .toList,
  pomPostProcess := { node =>
    import scala.xml._
    import scala.xml.transform._
    def stripIf(f: Node => Boolean) = new RewriteRule {
      override def transform(n: Node) =
        if (f(n)) NodeSeq.Empty else n
    }
    val stripTestScope = stripIf { n =>
      n.label == "dependency" && (n \ "scope").text == "test"
    }
    new RuleTransformer(stripTestScope).transform(node)(0)
  },
  releaseTagName := gitTagOrHash.value,
  releaseCrossBuild := true,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    releaseStepCommandAndRemaining(";scalafmtSbtCheck;scalafmtCheck;test:scalafmtCheck"),
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    updateReadmeProcess,
    tagRelease,
    releaseStepCross(PgpKeys.publishSigned),
    releaseStepCommandAndRemaining("iarrayNative/publishSigned"),
    setNextVersion,
    commitNextVersion,
    updateReadmeProcess,
    releaseStepCommand("sonatypeReleaseAll"),
    pushChanges
  ),
  TaskKey[Unit]("checkPackage", "show pom.xml and sources.jar") := {
    println(IO.read(makePom.value))
    println()
    IO.withTemporaryDirectory { dir =>
      IO.unzip((packageSrc in Compile).value, dir).map(f => f.getName -> f.length) foreach println
    }
  },
  scalaVersion := Scala211,
  crossScalaVersions := Scala211 :: "2.12.8" :: "2.13.0-RC1" :: Nil,
  name := "iarray",
  organization := "com.github.xuwei-k",
  startYear := Some(2014),
  description := "Immutable array wrapper. does not use ClassTag. scalaz friendly",
  scalacOptions in (Compile, doc) ++= {
    Seq(
      "-sourcepath",
      (baseDirectory in LocalRootProject).value.getAbsolutePath,
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
    "-Xlint",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazV
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
  buildInfoObject := "BuildInfoIArray"
).flatMap(_.settings)

val sonatypeURL =
  "https://oss.sonatype.org/service/local/repositories/"

val updateReadme: State => State = { state =>
  val extracted = Project.extract(state)
  val scalaV = "2.12"
  val v = extracted get version
  val org = extracted get organization
  val n = "iarray"
  val snapshotOrRelease = if (extracted get isSnapshot) "snapshots" else "releases"
  val readme = "README.md"
  val readmeFile = file(readme)
  val newReadme = Predef
    .augmentString(IO.read(readmeFile))
    .lines
    .map { line =>
      val matchReleaseOrSnapshot = line.contains("SNAPSHOT") == v.contains("SNAPSHOT")
      if (line.startsWith("libraryDependencies") && matchReleaseOrSnapshot) {
        if (line.contains(" %%% ")) {
          s"""libraryDependencies += "${org}" %%% "${n}" % "$v""""
        } else {
          s"""libraryDependencies += "${org}" %% "${n}" % "$v""""
        }
      } else if (line.contains(sonatypeURL) && matchReleaseOrSnapshot) {
        val n = extracted get (name in LocalRootProject)
        val sxrIndexHtml = "-sxr.jar/!/index.html"
        val javadocHtml = "-javadoc.jar/!/"
        val baseURL =
          s"${sonatypeURL}${snapshotOrRelease}/archive/${org.replace('.', '/')}/${n}_${scalaV}/${v}/${n}_${scalaV}-${v}"
        if (line.contains(javadocHtml)) {
          s"- [API Documentation](${baseURL}${javadocHtml}iarray/IArray.html)"
        } else if (line.contains(sxrIndexHtml)) {
          s"- [sxr](${baseURL}${sxrIndexHtml})"
        } else line
      } else line
    }
    .mkString("", "\n", "\n")
  IO.write(readmeFile, newReadme)
  val git = new Git(extracted get baseDirectory)
  git.add(readme) ! state.log
  git.commit(message = "update " + readme, sign = false, signOff = false) ! state.log
  sys.process.Process("git diff HEAD^") ! state.log
  state
}

commands += Command.command("updateReadme")(updateReadme)

val updateReadmeProcess: ReleaseStep = updateReadme

val enableSxr = SettingKey[Boolean]("enableSxr")
val packageSxr = TaskKey[File]("packageSxr")

def ifSxrAvailable[A](key: SettingKey[A], value: Def.Initialize[A]): Setting[A] =
  key := {
    if (enableSxr.value) {
      value.value
    } else {
      key.value
    }
  }

def ifSxrAvailable[A](key: TaskKey[A], value: Def.Initialize[Task[A]]): Setting[Task[A]] =
  key := {
    if (enableSxr.value) {
      value.value: @sbtUnchecked
    } else {
      key.value: @sbtUnchecked
    }
  }

val iarray = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CustomCrossType)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    commonSettings,
    scalapropsCoreSettings,
    scalapropsVersion := "0.6.0",
    libraryDependencies ++= Seq(
      ("com.github.scalaprops" %%% "scalaprops" % scalapropsVersion.value % "test"),
      ("com.github.scalaprops" %%% "scalaprops-scalaz" % scalapropsVersion.value % "test")
    )
  )
  .configurePlatforms(NativePlatform, JSPlatform)(
    _.disablePlugins(DoctestPlugin)
  )
  .jsSettings(
    scalacOptions += {
      val a = (baseDirectory in LocalRootProject).value.toURI.toString
      val g = "https://raw.githubusercontent.com/xuwei-k/iarray/" + gitTagOrHash.value
      s"-P:scalajs:mapSourceURI:$a->$g/"
    }
  )
  .jvmSettings(
    libraryDependencies ++= {
      if (scalaVersion.value == "2.13.0-RC2")
        Nil
      else
        Seq(
          "org.scalacheck" %% "scalacheck" % "1.14.0" % "test" // use in doctest
        )
    },
    enableSxr := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) =>
          v <= 12
        case _ =>
          false
      }
    },
    Defaults.packageTaskSettings(
      packageSxr in Compile,
      (crossTarget in Compile).map { dir =>
        Path.allSubpaths(dir / "classes.sxr").toSeq
      }
    ),
    ifSxrAvailable(
      resolvers,
      Def.setting(resolvers.value :+ ("bintray/paulp" at "https://dl.bintray.com/paulp/maven"))
    ),
    ifSxrAvailable(
      libraryDependencies,
      Def.setting(libraryDependencies.value :+ compilerPlugin("org.improving" %% "sxr" % "1.0.2"))
    ),
    ifSxrAvailable(
      packagedArtifacts,
      Def.task(packagedArtifacts.value ++ Classpaths.packaged(Seq(packageSxr in Compile)).value)
    ),
    ifSxrAvailable(
      artifacts,
      Def.setting(artifacts.value ++ Classpaths.artifactDefs(Seq(packageSxr in Compile)).value)
    ),
    ifSxrAvailable(
      artifactClassifier in packageSxr,
      Def.setting(Option("sxr"))
    )
  )
  .nativeSettings(
    scalapropsNativeSettings,
    scalaVersion := Scala211,
    crossScalaVersions := Scala211 :: Nil,
    selectMainClass in Test := Some("scalaprops.NativeTestMain")
  )

val iarrayJVM = iarray.jvm
val iarrayJS = iarray.js
val iarrayNative = iarray.native

val root = project
  .in(file("."))
  .aggregate(
    iarrayJVM,
    iarrayJS // exclude iarrayNative on purpose
  )
  .settings(
    commonSettings,
    scalaSource in Compile := file("dummy"),
    scalaSource in Test := file("dummy"),
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    PgpKeys.publishSigned := {},
    PgpKeys.publishLocalSigned := {}
  )
