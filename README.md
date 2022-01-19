# IArray

[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.xuwei-k/iarray_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.xuwei-k/iarray_2.12)
[![scaladoc](https://javadoc.io/badge2/com.github.xuwei-k/iarray_2.13/javadoc.svg)](https://javadoc.io/doc/com.github.xuwei-k/iarray_2.13/latest/iarray/index.html)

`IArray` is an Immutable Array wrapper for Scala. This library does __NOT__ use [scala.reflect.ClassTag](https://github.com/scala/scala/blob/v2.12.13/src/library/scala/reflect/ClassTag.scala)

[`scalaz.ImmutableArray`](https://github.com/scalaz/scalaz/blob/v7.3.3/core/src/main/scala/scalaz/ImmutableArray.scala) and [`scala.Array`](https://github.com/scala/scala/blob/v2.12.13/src/library/scala/Array.scala) could __NOT__ be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/v7.3.3/core/src/main/scala/scalaz/Functor.scala).
On the other hand `IArray` can be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/v7.3.3/core/src/main/scala/scalaz/Functor.scala), [`scalaz.Monad`](https://github.com/scalaz/scalaz/blob/v7.3.3/core/src/main/scala/scalaz/Monad.scala), [`scalaz.Traverse`](https://github.com/scalaz/scalaz/blob/v7.3.3/core/src/main/scala/scalaz/Traverse.scala) etc.

`IArray` __always boxing__ primitive values in return for provide an abstraction(scalaz typeclasses).


- [Maven Central Repository Search](https://search.maven.org/search?q=g:com.github.xuwei-k)
- [Maven Central](https://repo1.maven.org/maven2/com/github/xuwei-k/)

### latest stable version

```scala
libraryDependencies += "com.github.xuwei-k" %% "iarray" % "0.7.0"
```

for scala-js, scala-native

```scala
libraryDependencies += "com.github.xuwei-k" %%% "iarray" % "0.7.0"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/xuwei-k/iarray_2.12/0.7.0/iarray_2.12-0.7.0-javadoc.jar/!/iarray/IArray.html)
