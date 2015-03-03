# IArray

[![Build Status](https://secure.travis-ci.org/xuwei-k/iarray.png?branch=master)](http://travis-ci.org/xuwei-k/iarray)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.xuwei-k/iarray_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.xuwei-k/iarray_2.11)
[![Scaladoc](http://javadoc-badge.appspot.com/com.github.xuwei-k/iarray_2.11.svg?label=scaladoc)](http://javadoc-badge.appspot.com/com.github.xuwei-k/iarray_2.11)

`IArray` is an Immutable Array wrapper for Scala. This library does __NOT__ use [scala.reflect.ClassTag](https://github.com/scala/scala/blob/v2.11.6/src/library/scala/reflect/ClassTag.scala)

[`scalaz.ImmutableArray`](https://github.com/scalaz/scalaz/blob/v7.1.1/core/src/main/scala/scalaz/ImmutableArray.scala) and [`scala.Array`](https://github.com/scala/scala/blob/v2.11.6/src/library/scala/Array.scala) could __NOT__ be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/v7.1.1/core/src/main/scala/scalaz/Functor.scala).
On the other hand `IArray` can be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/v7.1.1/core/src/main/scala/scalaz/Functor.scala), [`scalaz.Monad`](https://github.com/scalaz/scalaz/blob/v7.1.1/core/src/main/scala/scalaz/Monad.scala), [`scalaz.Traverse`](https://github.com/scalaz/scalaz/blob/v7.1.1/core/src/main/scala/scalaz/Traverse.scala) etc.

`IArray` __always boxing__ primitive values in return for provide an abstraction(scalaz typeclasses).


- [Maven Central Repository Search](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.github.xuwei-k%22)
- [Maven Central](http://repo1.maven.org/maven2/com/github/xuwei-k/)

### latest stable version

```scala
libraryDependencies += "com.github.xuwei-k" %% "iarray" % "0.2.10"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/xuwei-k/iarray_2.11/0.2.10/iarray_2.11-0.2.10-javadoc.jar/!/index.html#iarray.IArray)

### snapshot version

```scala
resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies += "com.github.xuwei-k" %% "iarray" % "0.2.11-SNAPSHOT"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/xuwei-k/iarray_2.11/0.2.11-SNAPSHOT/iarray_2.11-0.2.11-SNAPSHOT-javadoc.jar/!/index.html#iarray.IArray)
- [sxr](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/xuwei-k/iarray_2.11/0.2.11-SNAPSHOT/iarray_2.11-0.2.11-SNAPSHOT-sxr.jar/!/index.html)


### for Scalaz 7.0.x

- <https://github.com/xuwei-k/iarray/tree/scalaz70#readme>



I'm not good at English. Documentation pull requests are welcome.
