# IArray

[![Build Status](https://secure.travis-ci.com/xuwei-k/iarray.png?branch=master)](http://travis-ci.com/xuwei-k/iarray)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.xuwei-k/iarray_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.xuwei-k/iarray_2.12)
[![Scaladoc](https://javadoc-badge.appspot.com/com.github.xuwei-k/iarray_2.12.svg?label=scaladoc)](https://javadoc-badge.appspot.com/com.github.xuwei-k/iarray_2.12/iarray/index.html?javadocio=true)

`IArray` is an Immutable Array wrapper for Scala. This library does __NOT__ use [scala.reflect.ClassTag](https://github.com/scala/scala/blob/v2.12.11/src/library/scala/reflect/ClassTag.scala)

[`scalaz.ImmutableArray`](https://github.com/scalaz/scalaz/blob/v7.2.27/core/src/main/scala/scalaz/ImmutableArray.scala) and [`scala.Array`](https://github.com/scala/scala/blob/v2.12.11/src/library/scala/Array.scala) could __NOT__ be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/v7.2.27/core/src/main/scala/scalaz/Functor.scala).
On the other hand `IArray` can be [`scalaz.Functor`](https://github.com/scalaz/scalaz/blob/v7.2.27/core/src/main/scala/scalaz/Functor.scala), [`scalaz.Monad`](https://github.com/scalaz/scalaz/blob/v7.2.27/core/src/main/scala/scalaz/Monad.scala), [`scalaz.Traverse`](https://github.com/scalaz/scalaz/blob/v7.2.27/core/src/main/scala/scalaz/Traverse.scala) etc.

`IArray` __always boxing__ primitive values in return for provide an abstraction(scalaz typeclasses).


- [Maven Central Repository Search](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.github.xuwei-k%22)
- [Maven Central](http://repo1.maven.org/maven2/com/github/xuwei-k/)

### latest stable version

```scala
libraryDependencies += "com.github.xuwei-k" %% "iarray" % "0.6.0"
```

for scala-js, scala-native

```scala
libraryDependencies += "com.github.xuwei-k" %%% "iarray" % "0.6.0"
```

- [API Documentation](https://oss.sonatype.org/service/local/repositories/releases/archive/com/github/xuwei-k/iarray_2.12/0.6.0/iarray_2.12-0.6.0-javadoc.jar/!/iarray/IArray.html)
