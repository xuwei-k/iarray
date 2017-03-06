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
      value.value
    } else {
      key.value
    }
  }

enableSxr := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, v)) =>
      v <= 12
    case _ =>
      false
  }
}

Defaults.packageTaskSettings(
  packageSxr in Compile, (crossTarget in Compile).map{ dir =>
    Path.allSubpaths(dir / "classes.sxr").toSeq
  }
)

ifSxrAvailable(
  resolvers,
  Def.setting(resolvers.value :+ ("bintray/paulp" at "https://dl.bintray.com/paulp/maven"))
)

ifSxrAvailable(
  libraryDependencies,
  Def.setting(libraryDependencies.value :+ compilerPlugin("org.improving" %% "sxr" % "1.0.2"))
)

ifSxrAvailable(
  packagedArtifacts,
  Def.task(packagedArtifacts.value ++ Classpaths.packaged(Seq(packageSxr in Compile)).value)
)

ifSxrAvailable(
  artifacts,
  Def.setting(artifacts.value ++ Classpaths.artifactDefs(Seq(packageSxr in Compile)).value)
)

ifSxrAvailable(
  artifactClassifier in packageSxr,
  Def.setting(Option("sxr"))
)
