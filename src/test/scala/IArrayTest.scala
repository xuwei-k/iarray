package iarray

import scalaz._
import std.tuple._, std.anyVal._, std.string._
import std.vector._, std.list._, std.option._, std.either._
import org.scalacheck._
import org.scalacheck.Prop.forAll
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._

object IArrayTraverseTest extends SpecLite {
  import IArrayTest._

  for ((name, prop) <- traverse.laws[IArray].properties) yield {
    property(name) = prop.check(new Test.Parameters.Default{
      override val maxSize = 5
    })
  }
}

object IArrayTest extends SpecLite {

  sealed trait AlphaTag
  type Alpha = String @@ AlphaTag

  implicit val alpha: Arbitrary[Alpha] =
    Tag.subst(Arbitrary(Gen.alphaStr))

  implicit def arb[A: Arbitrary]: Arbitrary[IArray[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[List[A]]])(IArray.fromList[A])

  implicit def iarrayShow[A: Show]: Show[IArray[A]] =
    Show.showA

  checkAll(monadPlus.strongLaws[IArray])
  checkAll(isEmpty.laws[IArray])
  checkAll(foldable.laws[IArray])
  checkAll(zip.laws[IArray])
  checkAll(align.laws[IArray])
  checkAll(cobind.laws[IArray])
  checkAll(monoid.laws[IArray[String]])

  val f = (_: Int) > 100

  property("unfold") = {
    val a = IArray.unfold(0)(a =>
      if(a < 10) Some((a + 1, a + 1)) else None
    ).toList
    val b = List.iterate(1, 10)(_ + 1)
    a must_=== b
  }

  property("toNel") = forAll { a: IArray[Byte] =>
    import syntax.std.list._
    a.toNel must_=== a.toList.toNel
  }

  property("oneAnd toOneAnd") = forAll { a: IArray[Byte] =>
    a.oneAnd must_=== a.toNel.map{ nel =>
      OneAnd(nel.head, IArray.from(nel.tail))
    }
    a.toOneAnd[IArray] must_=== a.oneAnd
    a.toOneAnd[List].map{
      case OneAnd(h, t) => NonEmptyList.nel(h, t)
    } must_=== a.toNel
  }

  property("iterate") = forAll { (z: Int, size: Byte) =>
    IArray.iterate(z, size)(_ + 2).toList must_=== List.iterate(z, size)(_ + 2)
  }

  property("tabulate") = forAll { size: Byte =>
    IArray.tabulate(size)(conforms) must_=== List.tabulate(size)(conforms).to[IArray]
  }

  property("CanBuildFrom") = forAll { (a: List[Int], b: List[String]) =>
    a.to[IArray].toList must_=== a
    b.to[IArray].toList must_=== b
  }

  property("zip zipWith") = forAll { (a: IArray[Int], b: IArray[String]) =>
    a.zip(b) must_=== a.toList.zip(b.toList).to[IArray]
    val f = (aa: Int, bb: String) => if(aa % 2 == 0) \/-(aa) else -\/(bb)
    a.zipWith(b)(f).toList must_=== (a.toList, b.toList).zipped.map(f)
  }

  property("unzip") = forAll { a: IArray[(Int, String)] =>
    val (left1, right1) = a.unzip
    val (left2, right2) = a.toList.unzip
    left1.toList  must_=== left2
    right1.toList must_=== right2
  }

  property("zipWithIndex") = forAll { a: IArray[Int] =>
    a.zipWithIndex.toList must_=== a.toList.zipWithIndex
  }

  property("flatten") = forAll { a: IArray[IArray[String]] =>
    a.flatten.toList must_=== a.map(_.toList).toList.flatten
  }

  property("exists") = forAll { a: IArray[Int] =>
    a.exists(f) must_=== a.toList.exists(f)
  }

  property("toArray Int") = forAll { a: IArray[Int] =>
    a.toArray.toList must_=== a.toList
  }

  property("to") = forAll { a: IArray[Int] =>
    a.to[Vector].toList must_=== a.toList
  }

  property("toArray String") = forAll { a: IArray[String] =>
    a.toArray.toList must_=== a.toList
  }

  property("toIterator") = forAll { a: IArray[String] =>
    a.toIterator.toList must_=== a.toList
  }

  property("filter") = forAll { a: IArray[Int] =>
    a.filter(f).toList must_=== a.toList.filter(f)
  }

  property("collect") = forAll { a: IArray[Int] =>
    val f: PartialFunction[Int, String] = {case i if i % 2 == 0 => (i * 3).toString}
    a.collect(f).toList must_=== a.toList.collect(f)
  }

  property("collectFirst") = forAll { a: IArray[Int] =>
    val f: PartialFunction[Int, String] = {case i if i % 2 == 0 => (i * 3).toString}
    a.collectFirst(f) must_=== a.toList.collectFirst(f)
  }

  property("find") = forAll { a: IArray[Int] =>
    a.find(f) must_=== a.toList.find(f)
  }

  property("flatMap") = forAll { a: IArray[Int] =>
    val f: Int => IArray[String] = {i: Int => IArray.single(i.toString)}
    a.flatMap(f).toList must_=== a.toList.flatMap(x => f(x).toList)
  }

  property("map") = forAll { a: IArray[Int] =>
    val f = (_:Int).toString
    a.map(f).toList must_=== a.toList.map(f)
  }

  property("foreach") = forAll { a: IArray[Int] =>
    val buf1, buf2 = new collection.mutable.ListBuffer[Int]
    val f1 = buf1 += (_:Int)
    val f2 = buf2 += (_:Int)
    a.foreach(f1)
    a.toList.foreach(f2)
    buf1.toList must_=== buf2.toList
  }

  property("cojoin cobind") = forAll { a: IArray[Int] =>
    val f = (x: List[Int]) => x.sum + x.length
    a.cojoin must_=== a.cobind(x => x)
    a.cojoin.map(_.toList).toList must_=== Cobind[List].cojoin(a.toList)
    a.cobind(x => f(x.toList)).toList must_=== Cobind[List].cobind(a.toList)(f)
  }

  property("reverse.reverse") = forAll { a: IArray[Int] =>
    a.reverse.reverse must_=== a
  }

  property("reverse") = forAll { a: IArray[Int] =>
    a.reverse.toList must_=== a.toList.reverse
  }

  property("reverse_:::") = forAll { (a: IArray[Int], b: IArray[Int]) =>
    (a reverse_::: b).toList must_=== (a.toList reverse_::: b.toList)
  }

  property("take") = forAll { (a: IArray[Int], n: Int) =>
    a.takeL(n).toList must_=== a.toList.take(n)
  }

  property("takeWhileL") = forAll { a: IArray[Int] =>
    val f = {i: Int => 5 < i }
    a.takeWhileL(f).toList must_=== a.toList.takeWhile(f)
  }

  property("takeWhileR") = forAll { a: IArray[Int] =>
    val f = {i: Int => 5 < i }
    a.takeWhileR(f).toList must_=== a.toList.reverse.takeWhile(f).reverse
  }

  property("takeR") = forAll { (a: IArray[Int], n: Int) =>
    a.takeR(n).toList must_=== a.toList.takeRight(n)
  }

  property("count") = forAll { a: IArray[Int] =>
    a.count(f) must_=== a.toList.count(f)
  }

  property("dropL") = forAll { (a: IArray[Int], n: Int) =>
    a.dropL(n).toList must_=== a.toList.drop(n)
  }

  property("dropWhileL") = forAll { a: IArray[Int] =>
    val f = {i: Int => i > 3}
    a.dropWhileL(f).toList must_=== a.toList.dropWhile(f)
  }

  property("dropR") = forAll { (a: IArray[Int], n: Int) =>
    a.dropR(n).toList must_=== a.toList.dropRight(n)
  }

  property("contains") = forAll { (a: IArray[Int], n: Int) =>
    a.contains(n) must_=== a.toList.contains(n)
  }

  property("splitAt") = forAll { (a: IArray[Int], n: Int) =>
    val (x1, x2) = a.splitAt(n)
    val (y1, y2) = a.toList.splitAt(n)
    (x1.toList must_=== y1) && (x2.toList must_=== y2)
  }

  property("++") = forAll { (a: IArray[Int], b: IArray[Int]) =>
    (a ++ b).toList must_=== (a.toList ++ b.toList)
  }

  property("+: Int") = forAll { (a: IArray[Int], b: Int) =>
    (b +: a).toList must_=== (b +: a.toList)
  }

  property("+: String") = forAll { (a: IArray[String], b: String) =>
    (b +: a).toList must_=== (b +: a.toList)
  }

  property(":+ Int") = forAll { (a: IArray[Int], b: Int) =>
    (a :+ b).toList must_=== (a.toList :+ b)
  }

  property(":+ String") = forAll { (a: IArray[String], b: String) =>
    (a :+ b).toList must_=== (a.toList :+ b)
  }

  property("forall") = forAll { a: IArray[Int] =>
    val f = {i: Int => 5 < i }
    a.forall(f) must_=== a.toList.forall(f)
  }

  property("dropWhileR") = forAll { a: IArray[Int] =>
    val f = {i: Int => i > 10}
    a.dropWhileR(f).toList must_=== a.toList.reverse.dropWhile(f).reverse
  }

  property("span") = forAll { a: IArray[Int] =>
    val f = {i: Int => i > 0}
    val (x1, x2) = a.span(f)
    val (y1, y2) = a.toList.span(f)
    (x1.toList must_=== y1) && (x2.toList must_=== y2)
  }

  property("partition") = forAll { a: IArray[Int] =>
    val f = {i: Int => i > 0}
    val (x1, x2) = a.partition(f)
    val (y1, y2) = a.toList.partition(f)
    (x1.toList must_=== y1) && (x2.toList must_=== y2)
  }

  property("updated") = forAll { (a: IArray[Int], index: Int, elem: Int) =>
    if(0 <= index && index < a.length)
      a.updated(index, elem).toList must_=== a.toList.updated(index, elem)
    else
      a.updated(index, elem).mustThrowA[IndexOutOfBoundsException]
  }

  property("indexOfL") = forAll { (a: IArray[Int], z: Int) =>
    a.indexOfL(z) must_=== Option(a.toList.indexOf(z)).filter(_ >= 0)
  }

  property("indexOfR") = forAll { (a: IArray[Int], z: Int) =>
    a.indexOfR(z) must_=== Option(a.toList.lastIndexOf(z)).filter(_ >= 0)
  }

  property("sorted Int") = forAll { a: IArray[Int] =>
    a.sorted.toList must_=== a.toList.sorted
  }

  property("sorted String") = forAll { a: IArray[String] =>
    a.sorted.toList must_=== a.toList.sorted
  }

  property("from") = forAll { a: List[Int] =>
    IArray.from(a).toList must_=== a
    IArray.fromList(a).toList must_=== a
    IArray.from(a.toVector).toList must_=== a
    IArray.fromIndexedSeq(a.toVector).toList must_=== a
  }

  property("reverseMap") = forAll { a: IArray[Int] =>
    val f1 = (_:Int).toString
    val f2 = (_:Int).toByte
    a.reverseMap(f1).toList must_=== a.toList.reverseMap(f1)
    a.reverseMap(f2).toList must_=== a.toList.reverseMap(f2)
  }

  property("slice") = forAll { (a: IArray[Int], from: Int, until: Int) =>
    a.slice(from, until).toList must_=== a.toList.slice(from, until)
  }

  property("fold") = forAll { a: IArray[List[Int]] =>
    Foldable[IArray].fold(a) must_=== Foldable[List].fold(a.toList)
  }

  property("foldl foldl1") = forAll { (a: IArray[Int], z: Int) =>
    a.foldl(z)(_ - _) must_=== a.toList.foldLeft(z)(_ - _)
    a.foldl1(_ - _) must_=== a.toList.reduceLeftOption(_ - _)
  }

  property("foldr foldr1") = forAll { (a: IArray[Int], z: List[Int]) =>
    a.foldr(z)(_ :: _) must_=== a.toList.foldRight(z)(_ :: _)
    a.foldr1(_ - _) must_=== a.toList.reduceRightOption(_ - _)
  }

  property("foldMapL1") = forAll { a: IArray[Int] =>
    val z = (a: Int) => Vector(a)
    a.foldMapL1(z)(_ :+ _) must_=== {
      if(a.isEmpty) None
      else Some(a.tailOption.get.toList.foldLeft(z(a.headOption.get))(_ :+ _))
    }
  }

  property("foldMapR1") = forAll { a: IArray[Int] =>
    val z = (a: Int) => List(a)
    a.foldMapR1(z)(_ :: _) must_=== {
      if(a.isEmpty) None
      else Some(a.initOption.get.toList.foldRight(z(a.lastOption.get))(_ :: _))
    }
  }

  property("scanLeft") = forAll { (a: IArray[Int], z: Int) =>
    a.scanLeft(z)(_ - _).toList must_=== a.toList.scanLeft(z)(_ - _)
  }

  property("scanRight") = forAll { (a: IArray[Int], z: List[Int]) =>
    a.scanRight(z)(_ :: _).toList must_=== a.toList.scanRight(z)(_ :: _)
  }

  property("scanLeft1") = forAll { a: IArray[Int] =>
    a.scanLeft1(_ - _).toList must_=== (
      if(a.isEmpty) List()
      else a.toList.tail.scanLeft(a.headOption.get)(_ - _)
    )
  }

  property("scanRight1") = forAll { a: IArray[String] =>
    a.scanRight1(_ + _).toList must_=== (
      if(a.isEmpty) List()
      else a.toList.init.scanRight(a.lastOption.get)(_ + _)
    )
  }

  property("foldMap1Opt") = forAll { a: IArray[Int] =>
    val f = (x: Int) => Vector(x)
    a.foldMap1Opt(f) must_=== Foldable[List].foldMap1Opt(a.toList)(f).map(_.toVector)
  }

  property("tailOption") = forAll { a: IArray[Int] =>
    a.tailOption.map(_.toList) must_=== (
      if(a.isEmpty) None
      else Some(a.toList.tail)
    )
  }

  property("headOption") = forAll { a: IArray[Int] =>
    a.headOption must_=== a.toList.headOption
  }

  property("lastOption") = forAll { a: IArray[Int] =>
    a.lastOption must_=== a.toList.lastOption
  }

  property("initOption") = forAll { a: IArray[Int] =>
    a.initOption.map(_.toList) must_=== (
      if(a.isEmpty) None
      else Some(a.toList.init)
    )
  }

  property("sum") = forAll { a: IArray[Int] =>
    a.sum must_=== a.toIterator.sum
  }

  property("maxBy") = forAll { a: IArray[Int] =>
    a.maxBy(_.toString) must_=== (
      if(a.isEmpty) None
      else Some(a.toList.maxBy(_.toString))
    )
  }

  property("minBy") = forAll { a: IArray[Int] =>
    a.minBy(_.toString) must_=== (
      if(a.isEmpty) None
      else Some(a.toList.minBy(_.toString))
    )
  }

  property("max") = forAll { a: IArray[Int] =>
    if(a.isEmpty){
      a.max must_=== None
    }else{
      a.max must_=== Some(a.toList.max)
    }
  }

  property("min") = forAll { a: IArray[Int] =>
    if(a.isEmpty){
      a.min must_=== None
    }else{
      a.min must_=== Some(a.toList.min)
    }
  }

  property("intersperse") = forAll{ xs: IArray[Alpha] =>
    import syntax.std.list._
    xs.intersperse(",").toList must_=== xs.toList.intersperse(Tag(","))
  }

  property("intercalate") = forAll { xs: IArray[Alpha] =>
    xs.intercalate(" , ") must_=== Foldable[List].intercalate(xs.toList, " , ")
    xs.intercalate(",") must_=== xs.mkString(",")
    xs.intercalate(",") must_=== xs.intercalate1Opt(",").getOrElse(Monoid[String].zero)
  }

  property("toString") = forAll { xs: IArray[Int] =>
    xs.toString must_=== xs.toList.mkString("IArray(", ", ", ")")
  }

  property("mkString") = forAll { (xs: IArray[Int], a: String, b: String, c: String) =>
    xs.mkString(a, b, c) must_=== xs.toArray.mkString(a, b, c)
  }

  property("startsWith") = forAll { (a: IArray[Int], b: IArray[Int], n: Int) =>
   if(n >= 0){
     a.startsWith(b, n) must_=== a.toList.startsWith(b.toList, n)
   }else{
     a.startsWith(b, n).mustThrowA[IllegalArgumentException]
   }
  }

  property("endsWith") = forAll { (a: IArray[Int], b: IArray[Int]) =>
    a.endsWith(b) must_=== a.toList.endsWith(b.toList)
  }

  property("for comprehension") = forAll { (xs: IArray[Int], ys: IArray[String]) =>
    val a = for{ x <- xs; if x % 2 == 0; y <- ys } yield (x, y)
    val b = for{ x <- xs.toList; if x % 2 == 0; y <- ys.toList} yield (x, y)

    a must_=== b.to[IArray]

    val buf1, buf2 = List.newBuilder[(Int, String)]

    for{
      x <- xs.toList; if x % 2 == 0; y <- ys.toList; if true
    }{ buf1 += ((x, y)) }

    for{
      x <- xs; if x % 2 == 0; y <- ys; if true
    }{ buf2 += ((x, y)) }

    buf1.result must_=== buf2.result
  }

  property("separate Eithers, Validations") = forAll { eithers: IArray[Int \/ String] =>
    val F = Bifunctor[Tuple2]
    import F.bifunctorSyntax._
    MonadPlus[IArray].separate(eithers).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(eithers.toList)
    val validations = eithers.map(_.validation)
    MonadPlus[IArray].separate(validations).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(validations.toList)
    val stdEithers = eithers.map(_.toEither)
    MonadPlus[IArray].separate(stdEithers).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(stdEithers.toList)
  }

  property("separate Tuple2, LazyTuple2") = forAll { tuples: IArray[(Int, String)] =>
    val F = Bifunctor[Tuple2]
    import F.bifunctorSyntax._
    MonadPlus[IArray].separate(tuples).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(tuples.toList)
    val lazyTuples = tuples.map{case (a, b) => LazyTuple2(a, b)}
    MonadPlus[IArray].separate(lazyTuples).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(lazyTuples.toList)
  }

  property("separate These") = forAll { these: IArray[Int \&/ String] =>
    val F = Bifunctor[Tuple2]
    import F.bifunctorSyntax._
    MonadPlus[IArray].separate(these).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(these.toList)
  }

  property("partitionLazyTuples") = {
    IArray.partitionLazyTuples(IArray(LazyTuple2(1, sys.error("error"): String)))._1 must_=== IArray(1)
    IArray.partitionLazyTuples(IArray(LazyTuple2(sys.error("error"): Int, "a")))._2  must_=== IArray("a")
  }

  property("groupBy1") = forAll { (xs: IArray[Int], x: Int) =>
    val f = (a: Int) => if(x == 0) a else a % x
    xs.groupBy1[Int, Int](f).map{
      case OneAnd(h, t) => NonEmptyList.nel(h, t.toList)
    } must_=== ==>>.fromList(std.list.groupBy1(xs.toList)(f).toList)
  }

  property("traverseS") = forAll { (xs: IArray[Int], n: Int, z: Int) =>
    val F = Traverse[List]
    import F.traverseSyntax._
    val f = (a: Int) => State.gets[Int, Int](b => if(b % 2 == 0) b - a + n else a - b )
    val x = Traverse[IArray].traverseS(xs)(f)(z)
    x must_=== Bifunctor[Tuple2].rightMap(xs.toList.traverseS(f)(z))(_.to[IArray])

    val G = new Traverse[IArray] {
      def traverseImpl[G[_]: Applicative, A, B](fa: iarray.IArray[A])(f: A => G[B]) =
        Traverse[IArray].traverseImpl(fa)(f)
    }

    x must_=== G.traverseS(xs)(f)(z)
  }

}

