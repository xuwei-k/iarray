resolvers += Opts.resolver.sonatypeReleases

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions"
)

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.7")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")

addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.7.0")

addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.2.5")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.22")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.3.1")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.6")

addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.4.0")
