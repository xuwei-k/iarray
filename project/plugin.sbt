resolvers += Opts.resolver.sonatypeReleases

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions"
)

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.5")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.6.0")

addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.2.3")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.18")

addSbtPlugin("org.scala-native" % "sbt-scalajs-crossproject" % "0.2.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.1")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "0.6.8")
