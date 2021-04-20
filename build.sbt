import sbtcrossproject.crossProject
import sbtrelease._
import ReleaseStateTransformations._

def releaseStepCross[A](key: TaskKey[A]) =
  ReleaseStep(
    action = { state =>
      val extracted = Project extract state
      extracted.runAggregated(extracted.get(thisProjectRef) / (Global / key), state)
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

val scalazV = "7.3.3"

lazy val gitTagOrHash = Def.setting {
  if (isSnapshot.value) {
    sys.process.Process("git rev-parse HEAD").lineStream_!.head
  } else {
    "v" + version.value
  }
}

val commonSettings = Seq[SettingsDefinition](
  publishTo := sonatypePublishToBundle.value,
  (Compile / unmanagedResources) += (LocalRootProject / baseDirectory).value / "LICENSE.txt",
  Global / credentials ++= PartialFunction
    .condOpt(sys.env.get("SONATYPE_USER") -> sys.env.get("SONATYPE_PASS")) { case (Some(user), Some(pass)) =>
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass)
    }
    .toList,
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
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    updateReadmeProcess,
    tagRelease,
    releaseStepCross(PgpKeys.publishSigned),
    releaseStepCommandAndRemaining("+ iarrayNative/publishSigned"),
    releaseStepCommandAndRemaining("sonatypeBundleRelease"),
    setNextVersion,
    commitNextVersion,
    updateReadmeProcess,
    pushChanges
  ),
  TaskKey[Unit]("checkPackage", "show pom.xml and sources.jar") := {
    println(IO.read(makePom.value))
    println()
    IO.withTemporaryDirectory { dir =>
      IO.unzip((Compile / packageSrc).value, dir).map(f => f.getName -> f.length) foreach println
    }
  },
  scalaVersion := Scala211,
  crossScalaVersions := Scala211 :: "2.12.13" :: "2.13.5" :: "3.0.0-RC3" :: Nil,
  name := "iarray",
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
    "-language:existentials,higherKinds,implicitConversions"
  ),
  scalacOptions ++= {
    if (isDotty.value) {
      Nil
    } else {
      Seq(
        "-Xsource:3",
        "-Xlint"
      )
    }
  },
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % scalazV
  ).map(_.withDottyCompat(scalaVersion.value)),
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
        val n = extracted.get(LocalRootProject / name)
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
    scalapropsVersion := "0.8.2",
    libraryDependencies ++= Seq(
      ("com.github.scalaprops" %%% "scalaprops" % scalapropsVersion.value % "test"),
      ("com.github.scalaprops" %%% "scalaprops-scalaz" % scalapropsVersion.value % "test")
    ).map(_.withDottyCompat(scalaVersion.value))
  )
  .configurePlatforms(NativePlatform, JSPlatform)(
    _.disablePlugins(DoctestPlugin)
  )
  .jsSettings(
    scalacOptions ++= {
      val a = (LocalRootProject / baseDirectory).value.toURI.toString
      val g = "https://raw.githubusercontent.com/xuwei-k/iarray/" + gitTagOrHash.value
      if (isDottyJS.value) {
        Seq(s"-scalajs-mapSourceURI:$a->$g/")
      } else {
        Seq(s"-P:scalajs:mapSourceURI:$a->$g/")
      }
    }
  )
  .jvmSettings(
    libraryDependencies ++= {
      Seq(
        "org.scalacheck" %% "scalacheck" % "1.15.2" % "test" // use in doctest
      ).map(_.withDottyCompat(scalaVersion.value))
    },
    enableSxr := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, v)) =>
          v <= 11
        case _ =>
          false
      }
    },
    Defaults.packageTaskSettings(
      (Compile / packageSxr),
      (Compile / crossTarget).map { dir => Path.allSubpaths(dir / "classes.sxr").toSeq }
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
      Def.task(packagedArtifacts.value ++ Classpaths.packaged(Seq(Compile / packageSxr)).value)
    ),
    ifSxrAvailable(
      artifacts,
      Def.setting(artifacts.value ++ Classpaths.artifactDefs(Seq(Compile / packageSxr)).value)
    ),
    ifSxrAvailable(
      packageSxr / artifactClassifier,
      Def.setting(Option("sxr"))
    )
  )
  .nativeSettings(
    scalapropsNativeSettings,
    crossScalaVersions ~= (_.filter(_ startsWith "2.1"))
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
    Compile / scalaSource := baseDirectory.value / "dummy",
    Test / scalaSource := baseDirectory.value / "dummy",
    publishArtifact := false,
    publish := {},
    publishLocal := {},
    PgpKeys.publishSigned := {},
    PgpKeys.publishLocalSigned := {}
  )
