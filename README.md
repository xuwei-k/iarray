# IArray [![Build Status](https://secure.travis-ci.org/xuwei-k/iarray.png?branch=master)](http://travis-ci.org/xuwei-k/iarray)

`IArray` is an Immutable Array wrapper for Scala. This library does __NOT__ use [scala.reflect.ClassTag](https://github.com/scala/scala/blob/v2.11.1/src/library/scala/reflect/ClassTag.scala)

[`scalaz.ImmutableArray`](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/ImmutableArray.scala) and [`scala.Array`](https://github.com/scala/scala/blob/v2.11.1/src/library/scala/Array.scala) could __NOT__ be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/Functor.scala).
On the other hand `IArray` can be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/Functor.scala), [`scalaz.Monad`](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/Monad.scala), [`scalaz.Traverse`](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/Traverse.scala) etc.

`IArray` __always boxing__ primitive values in return for provide an abstraction(scalaz typeclasses).


- [Maven Central Repository Search](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.github.xuwei-k%22)
- [Maven Central](http://repo1.maven.org/maven2/com/github/xuwei-k/)

### latest stable version

```scala
libraryDependencies += "com.github.xuwei-k" %% "iarray" % "0.2.8"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/xuwei-k/iarray_2.11/0.2.8/iarray_2.11-0.2.8-javadoc.jar/!/index.html#iarray.IArray)

### snapshot version

```scala
resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies += "com.github.xuwei-k" %% "iarray" % "0.2.9-SNAPSHOT"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/snapshots/archive/com/github/xuwei-k/iarray_2.11/0.2.9-SNAPSHOT/iarray_2.11-0.2.9-SNAPSHOT-javadoc.jar/!/index.html#iarray.IArray)


### for Scalaz 7.0.x

- <https://github.com/xuwei-k/iarray/tree/scalaz70#readme>



I'm not good at English. Documentation pull requests are welcome.
