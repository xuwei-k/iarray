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

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.5")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.4.0")

addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.2.3")

fullResolvers ~= {_.filterNot(_.name == "jcenter")}
