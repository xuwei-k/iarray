scalacOptions ++= Seq("-deprecation", "-Xlint", "-unchecked", "-language:_")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.1.5")

