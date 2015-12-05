resolvers += Opts.resolver.sonatypeReleases

scalacOptions ++= (
  "-deprecation" ::
  "-unchecked" ::
  "-Xlint" ::
  "-language:existentials" ::
  "-language:higherKinds" ::
  "-language:implicitConversions" ::
  Nil
)

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.2")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.3.2")

addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.3.4")

addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.1.0")

fullResolvers ~= {_.filterNot(_.name == "jcenter")}
