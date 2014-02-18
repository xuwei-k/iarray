import sbtrelease._
import ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys

releaseSettings

val sonatypeURL =
"https://oss.sonatype.org/service/local/repositories/releases/archive/"

val updateReadme = { state: State =>
  val extracted = Project.extract(state)
  val scalaV = extracted get scalaBinaryVersion
  val v = extracted get version
  val org =  extracted get organization
  val n = extracted get name
  val readme = "README.md"
  val readmeFile = file(readme)
  val newReadme = Predef.augmentString(IO.read(readmeFile)).lines.map{ line =>
    if(line.startsWith("libraryDependencies")){
      s"""libraryDependencies += "${org}" %% "${n}" % "$v""""
    }else if(line contains sonatypeURL){
      s"- [API Documentation](${sonatypeURL}${org.replace('.','/')}/${n}_${scalaV}/${v}/${n}_${scalaV}-${v}-javadoc.jar/!/index.html#iarray.IArray)"
    }else line
  }.mkString("", "\n", "\n")
  IO.write(readmeFile, newReadme)
  val git = new Git(extracted get baseDirectory)
  git.add(readme) ! state.log
  git.commit("update " + readme) ! state.log
  "git diff HEAD^" ! state.log
  state
}

commands += Command.command("updateReadme")(updateReadme)

val updateReadmeProcess: ReleaseStep = updateReadme

val publishSignedStep: ReleaseStep = ReleaseStep(
  action = state => Project.extract(state).runTask(PgpKeys.publishSigned, state)._1,
  enableCrossBuild = true
)

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
    Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}

sonatypeSettings

val userPass = for {
  user <- sys.env.get("SONATYPE_USER")
  pass <- sys.env.get("SONATYPE_PASS")
} yield (user, pass)

credentials ++= userPass.map{
  case (user, pass) => Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, pass) :: Nil
}.getOrElse(sys.error("hoge"))

pomIncludeRepository := { _ => false }

publishArtifact in Test := false

