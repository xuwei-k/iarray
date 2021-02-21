scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions"
)

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")

addSbtPlugin("com.github.sbt" % "sbt-release" % "1.0.15")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.5")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")

addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.9.9")

addSbtPlugin("com.github.scalaprops" % "sbt-scalaprops" % "0.4.1")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.0")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.5.3")
