import sbtrelease._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys

bintrayPublishSettings

bintray.Keys.packageLabels in bintray.Keys.bintray := Seq("array", "collection", "scala")

bintray.Keys.whoami := "xuwei-k"

releaseSettings

val updateReadme = { (state: State, v: String) =>
  val readme = "README.md"
  val readmeFile = file(readme)
  val newReadme = Predef.augmentString(IO.read(readmeFile)).lines.map{ line =>
    if(line.startsWith("libraryDependencies")){
      s"""libraryDependencies += "com.github.xuwei-k" %% "iarray" % "$v""""
    }else line
  }.mkString("", "\n", "\n")
  IO.write(readmeFile, newReadme)
  Git.add(readme) ! state.log
  Git.commit("update " + readme) ! state.log
  "git diff HEAD^" ! state.log
  state
}

commands += Command.single("updateReadme")(updateReadme)

val updateReadmeProcess: ReleaseStep = { state: State =>
  val v = Project.extract(state) get version
  updateReadme(state, v)
}

val publishSignedStep: ReleaseStep = ReleaseStep{ state =>
  Project.extract(state).runTask(PgpKeys.publishSigned, state)._1
}

ReleaseKeys.releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  updateReadmeProcess,
  tagRelease,
  publishSignedStep,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

val checkPackage = taskKey[Unit]("show pom.xml and sources.jar")

checkPackage := {
  println(IO.read(makePom.value))
  println()
  IO.withTemporaryDirectory{ dir =>
    IO.unzip((packageSrc in Compile).value, dir).map(f => f.getName -> f.length) foreach println
  }
}

publishTo := {
  if(version.value endsWith "SNAPSHOT")
    Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
  else
    publishTo.value
}
