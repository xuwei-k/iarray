package iarray

import scalaz._
import std.tuple._, std.anyVal._, std.string._
import std.vector._, std.list._, std.option._, std.either._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object IArrayTest extends TestCommon{

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

  property("merge") = forAll { (a: IArray[Int], b: IArray[Int]) =>
    Align[IArray].merge(a, b).toList must_=== Align[List].merge(a.toList, b.toList)
  }

  property("zip zipWith") = forAll { (a: IArray[Int], b: IArray[String]) =>
    a.zip(b) must_=== a.toList.zip(b.toList).to[IArray]
    val f = (aa: Int, bb: String) => if(aa % 2 == 0) \/-(aa) else -\/(bb)
    a.zipWith(b)(f).toList must_=== (a.toList, b.toList).zipped.map(f)
    a.zip(b) must_=== IArray.zipApply.tuple2(a, b)
  }

  property("zipAll") = forAll { (as: IArray[Int], bs: IArray[String], a: Int, b: String) =>
    as.zipAll(bs, a, b).toList must_=== as.toList.zipAll(bs.toList, a, b)
  }

  property("zipApply") = {
    def undefined[A]: IArray[A] = sys.error("error")
    // TODO https://github.com/scalaz/scalaz/commit/b24d595957
    // IArray.zipApply.tuple3(IArray.empty[Int], sys.error(""): Int, sys.error(""): Int) must_=== IArray.empty[(Int, Int, Int)]

    IArray.zipApply.tuple2(IArray.empty[Int], undefined[Int]) must_=== IArray.empty[(Int, Int)]
    IArray.zipApply.tuple3(IArray.empty[Int], undefined[Int], IArray(1)) must_=== IArray.empty[(Int, Int, Int)]

    IArray.zipApply.apply2(IArray.empty[Int], undefined[Int])(Tuple2.apply) must_=== IArray.empty[(Int, Int)]
    IArray.zipApply.apply3(IArray.empty[Int], undefined[Int], undefined[Int])(Tuple3.apply) must_=== IArray.empty[(Int, Int, Int)]
    IArray.zipApply.apply3(IArray(1), IArray.empty[Int], undefined[Int])(Tuple3.apply) must_=== IArray.empty[(Int, Int, Int)]
  }

  property("zip3 zipWith3") = forAll { (a: IArray[Int], b: IArray[String], c: IArray[Long]) =>
    IArray.zip3(a, b, c) must_=== (a.toList, b.toList, c.toList).zipped.to[IArray]
    IArray.zipApply.tuple3(a, b, c) must_=== (a.toList, b.toList, c.toList).zipped.to[IArray]
    IArray.zipWith3(a, b, c)(T3) must_=== (a.toList, b.toList, c.toList).zipped.map(T3).to[IArray]
  }

  property("zip4 zipWith4") = forAll { (a: IArray[Int], b: IArray[Alpha], c: IArray[Long], d: IArray[List[Int]]) =>
    val x = Zip[List].ap.tuple4(a.toList, b.toList, c.toList, d.toList).to[IArray]
    IArray.zipApply.apply4(a, b, c, d)(Tuple4.apply) must_=== x
    IArray.zipApply.tuple4(a, b, c, d) must_=== x
  }

  property("unzip firsts seconds") = forAll { a: IArray[(Int, String)] =>
    val (left1, right1) = a.unzip
    val (left2, right2) = a.toList.unzip
    left1.toList  must_=== left2
    right1.toList must_=== right2

    left1  must_=== a.firsts
    right1 must_=== a.seconds
  }

  final case class T2(_1: Int, _2: String) extends Product2[Int, String]
  object T2 extends Function2[Int, String, T2] with ShowAndEq[T2]{
    implicit val a = Functor[Arbitrary].map(implicitly[Arbitrary[(Int, String)]])(T2.tupled)
  }
  final case class T3(_1: Int, _2: String, _3: Long) extends Product3[Int, String, Long]
  object T3 extends Function3[Int, String, Long, T3] with ShowAndEq[T3]{
    implicit val a = Functor[Arbitrary].map(implicitly[Arbitrary[(Int, String, Long)]])(T3.tupled)
  }
  final case class T4(_1: String, _2: Int, _3: Long, _4: List[Int]) extends Product4[String, Int, Long, List[Int]]
  object T4 extends Function4[String, Int, Long, List[Int], T4] with ShowAndEq[T4]{
    implicit val a = Functor[Arbitrary].map(implicitly[Arbitrary[(String, Int, Long, List[Int])]])(T4.tupled)
  }

  property("unzip3") = forAll { a: IArray[T3] =>
    val (_1, _2, _3) = a.unzip3
    (_1.toList, _2.toList, _3.toList) must_=== a.toList.map(x => (x._1, x._2, x._3)).unzip3
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
  }

  property("unzip3") = forAll { a: IArray[(Int, String, Long)] =>
    val (_1, _2, _3) = a.unzip3
    (_1.toList, _2.toList, _3.toList) must_=== a.toList.unzip3
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
  }

  property("unzip4") = forAll { a: IArray[T4] =>
    val (_1, _2, _3, _4) = a.unzip4
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
    _4 must_=== a.map(_._4)
  }

  property("unzip4") = forAll { a: IArray[(Int, String, Long, List[Int])] =>
    val (_1, _2, _3, _4) = a.unzip4
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
    _4 must_=== a.map(_._4)
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

  property("collectBy") = forAll { a: IArray[Int \/ String] =>
    implicit val (s, e) = (Show.showA[\/-[String]], Equal.equalA[\/-[String]])
    a.collectBy[\/-[String]] must_=== a.collect{case r @ \/-(_) => r}
  }

  property("collect") = forAll { a: IArray[Int] =>
    var sideEffect = 0
    val f: PartialFunction[Int, String] = {case i if { sideEffect += 1; i % 2 == 0} => (i * 3).toString}
    a.collect(f).toList must_=== a.toList.collect(f)
    sideEffect must_=== (a.length * 2)
  }

  property("collect String") = forAll { a: IArray[Alpha] =>
    val f: PartialFunction[String, Int] = {case s if s.sum % 2 == 0 => s.length}
    a.collect(f).toList must_=== a.toList.collect(f)
  }

  property("collectFirst collectLast") = forAll { a: IArray[Int] =>
    val f: PartialFunction[Int, String] = {case i if i % 2 == 0 => (i * 3).toString}
    a.collectFirst(f) must_=== a.toList.collectFirst(f)
    a.collectLast(f) must_=== a.reverse.collectFirst(f)
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

  property("sortBy") = forAll { a: IArray[Int] =>
    a.sortBy(- _).toList must_=== a.toList.sortBy(- _)
    a.sortBy(_.toHexString).toList must_=== a.toList.sortBy(_.toHexString)
  }

  property("sortWith") = forAll { a: IArray[Int] =>
    a.sortWith(_ > _).toList must_=== a.toList.sortWith(_ > _)
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

  property("mapAccumL mapAccumR") = forAll { a: IArray[List[Int]] =>
    import syntax.bifunctor._
    val f = (acc: List[Int], x: List[Int]) => (x.sum :: x ::: acc, acc.sum)
    val F = Traverse[List]
    a.mapAccumL(List[Int]())(f).rightMap(_.toList) must_=== F.mapAccumL(a.toList, List[Int]())(f)
    a.mapAccumR(List[Int]())(f).rightMap(_.toList) must_=== F.mapAccumR(a.toList, List[Int]())(f)
  }

  property("fold") = forAll { a: IArray[List[Int]] =>
    Foldable[IArray].fold(a) must_=== Foldable[List].fold(a.toList)
  }

  property("fold1Opt") = forAll { a: IArray[NonEmptyList[Int]] =>
    a.fold1Opt must_=== Foldable[List].foldMap1Opt(a.toList)(conforms)
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

  property("foldMapR1Opt") = forAll { a: IArray[Int] =>
    val z = (a: Int) => List(a)
    a.foldMapR1Opt(z)(_ :: _) must_=== {
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

  property("partitionTry") = forAll { xs: IArray[scala.util.Try[Int]] =>
    import std.java.throwable._
    implicit val throwableEq = Equal.equalRef[Throwable]
    IArray.partitionTry(xs) must_=== IArray.partitionEithers(xs.map(tryEitherIso.from(_)))
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
    xs.groupBy1(f).map(_.toNel) must_=== ==>>.fromList(std.list.groupBy1(xs.toList)(f).toList)
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

  property("tailOptionEodo initOptionEndo") = forAll(
    implicitly[Arbitrary[IArray[Int]]].arbitrary,
    Gen.choose(-10, 10),
    Gen.choose(-10, 10)
  ){
    (xs, a, b) =>

    import syntax.monoid._
    val i = IArray.initOptionEndo[Int].multiply(b)
    val t = IArray.tailOptionEndo[Int].multiply(a)

    (i |+| t).run(xs) match {
      case Some(x) =>
        x must_=== xs.dropR(b).dropL(a)
      case None =>
        xs.length mustBe_< ((a max 0) + (b max 0))
    }
  }

  property("zipperEnd") = forAll { xs: IArray[Alpha] =>
    import syntax.std.list._
    xs.zipperEnd must_=== xs.toList.zipperEnd
  }

}

