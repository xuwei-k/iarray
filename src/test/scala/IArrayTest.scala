package iarray

import scalaz._
import std.tuple._, std.anyVal._, std.string._
import std.vector._, std.list._, std.option._, std.either._
import IArray.conform
import scalaprops._
import scalaprops.GenTags.AlphaNum
import scalaprops.Property.forAll

object IArrayTest extends TestCommon {

  val withIndex = forAll { a: IArray[Int] =>
    a.withIndex.map((x, y) => (x, y)) must_=== a.zipWithIndex
    a.withIndex.to[List] must_=== a.toList.zipWithIndex
  }

  val `WithFilter#to` = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.withFilter(f).to[List] must_=== a.toList.filter(f)
  }

  val `fromArray Int` = forAll { a: IArray[Int] =>
    IArray.fromArray(a.toArray) must_=== a
    IArray.fromArray(a.toArray).takeL(2) must_=== a.takeL(2)
  }

  val `fromArray Ref` = forAll { a: IArray[List[Int]] =>
    IArray.fromArray(a.toArray) must_=== a
    IArray.fromArray(a.toArray).takeL(2) must_=== a.takeL(2)
  }.mapSize(_ / 4)

  val fromRefArray = forAll { a: IArray[List[Int]] =>
    IArray.fromRefArray(a.toArray) must_=== a
  }.mapSize(_ / 4)

  val reversed = forAll { a: IArray[Int] =>
    a.reversed[List] must_=== a.reverse.toList
  }

  val reverseList = forAll { a: IArray[Int] =>
    a.reverseList must_=== a.reverse.toList
  }

  val reverseIList = forAll { a: IArray[Int] =>
    a.reverseIList must_=== a.reverse.toIList
  }

  val reverseArray = forAll { a: IArray[Int] =>
    a.reverseArray.toList must_=== a.reverse.toList
  }

  val unfold = forAll {
    val a = IArray.unfold(0)(a => if (a < 10) Some((a + 1, a + 1)) else None).toList
    val b = List.iterate(1, 10)(_ + 1)
    a must_=== b
  }

  val toIArray1 = forAll { a: IArray[Int] =>
    a.toIArray1 must_=== a.toNel.map(IArray1.fromNel)
  }

  val toNel = forAll { a: IArray[Byte] =>
    import syntax.std.list._
    a.toNel must_=== a.toList.toNel
  }

  val toIList = forAll { a: IList[String @@ AlphaNum] =>
    import syntax.foldable._
    a.to[IArray].toIList must_=== a
  }

  val `oneAnd toOneAnd` = forAll { a: IArray[Byte] =>
    a.oneAnd must_=== a.toNel.map { nel =>
      OneAnd(nel.head, IArray.fromIList(nel.tail))
    }
    a.toOneAnd[IArray] must_=== a.oneAnd
    a.toOneAnd[List].map {
      case OneAnd(h, t) => NonEmptyList.nel(h, IList.fromList(t))
    } must_=== a.toNel
  }

  val iterate = forAll { (z: Int, size: Byte, f: Int => Int) =>
    IArray.iterate(z, size)(f).toList must_=== List.iterate(z, size)(f)
  }

  val tabulate = forAll { size: Byte =>
    IArray.tabulate(size)(conform) must_=== List.tabulate(size)(conform).to[IArray]
  }

  val canBuildFrom = forAll { (a: List[Int], b: List[String]) =>
    a.to[IArray].toList must_=== a
    b.to[IArray].toList must_=== b
  }

  val merge = forAll { (a: IArray[Int], b: IArray[Int]) =>
    Align[IArray].merge(a, b).toList must_=== Align[List].merge(a.toList, b.toList)
  }

  val `zip zipWith` = forAll { (a: IArray[Int], b: IArray[String], f: (Int, String) => Int \/ Int) =>
    a.zip(b) must_=== a.toList.zip(b.toList).to[IArray]
    a.zipWith(b)(f).toList must_=== (a.toList, b.toList).zipped.map(f)
    a.zip(b) must_=== IArray.zipApply.tuple2(a, b)
  }

  val zipAll = forAll { (as: IArray[Int], bs: IArray[String], a: Int, b: String) =>
    as.zipAll(bs, a, b).toList must_=== as.toList.zipAll(bs.toList, a, b)
  }

  val zipApply = forAll {
    def undefined[A]: IArray[A] = sys.error("error")

    IArray.zipApply.tuple2(IArray.empty[Int], undefined[Int]) must_=== IArray.empty[(Int, Int)]
    IArray.zipApply.tuple3(IArray.empty[Int], undefined[Int], IArray(1)) must_=== IArray.empty[(Int, Int, Int)]
    IArray.zipApply.tuple3(IArray.empty[Int], undefined[Int], undefined[Int]) must_=== IArray.empty[(Int, Int, Int)]

    IArray.zipApply.apply2(IArray.empty[Int], undefined[Int])(Tuple2.apply) must_=== IArray.empty[(Int, Int)]
    IArray.zipApply.apply3(IArray.empty[Int], undefined[Int], undefined[Int])(Tuple3.apply) must_=== IArray
      .empty[(Int, Int, Int)]
    IArray.zipApply.apply3(IArray(1), IArray.empty[Int], undefined[Int])(Tuple3.apply) must_=== IArray
      .empty[(Int, Int, Int)]
  }

  val `zip3 zipWith3` = forAll { (a: IArray[Int], b: IArray[String], c: IArray[Long]) =>
    IArray.zip3(a, b, c) must_=== (a.toList, b.toList, c.toList).zipped.to[IArray]
    IArray.zipApply.tuple3(a, b, c) must_=== (a.toList, b.toList, c.toList).zipped.to[IArray]
    IArray.zipWith3(a, b, c)(T3) must_=== (a.toList, b.toList, c.toList).zipped.map(T3).to[IArray]
  }

  val `zip4 zipWith4` = forAll {
    (a: IArray[Int], b: IArray[String @@ AlphaNum], c: IArray[Long], d: IArray[List[Int]]) =>
      val x = Zip[List].ap.tuple4(a.toList, b.toList, c.toList, d.toList).to[IArray]
      IArray.zipApply.apply4(a, b, c, d)(Tuple4.apply) must_=== x
      IArray.zipApply.tuple4(a, b, c, d) must_=== x
  }

  val `zip5 zipWith5` = forAll {
    (a: IArray[Int], b: IArray[String @@ AlphaNum], c: IArray[Long], d: IArray[List[Int]], e: IArray[(Int, Int)]) =>
      val x = Zip[List].ap.tuple5(a.toList, b.toList, c.toList, d.toList, e.toList).to[IArray]
      IArray.zipApply.apply5(a, b, c, d, e)(Tuple5.apply) must_=== x
      IArray.zipApply.tuple5(a, b, c, d, e) must_=== x
  }

  val `unzip firsts seconds` = forAll { a: IArray[(Int, String)] =>
    val (left1, right1) = a.unzip
    val (left2, right2) = a.toList.unzip
    left1.toList must_=== left2
    right1.toList must_=== right2

    left1 must_=== a.firsts
    right1 must_=== a.seconds
  }

  final case class T2(_1: Int, _2: String) extends Product2[Int, String]
  object T2 extends Function2[Int, String, T2] with ShowAndEq[T2] {
    implicit val a = scalaprops.Gen[(Int, String)].map(T2.tupled)
  }
  final case class T3(_1: Int, _2: String, _3: Long) extends Product3[Int, String, Long]
  object T3 extends Function3[Int, String, Long, T3] with ShowAndEq[T3] {
    implicit val a: scalaprops.Gen[T3] = scalaprops.Gen[(Int, String, Long)].map(T3.tupled)
  }
  final case class T4(_1: String, _2: Int, _3: Long, _4: List[Int]) extends Product4[String, Int, Long, List[Int]]
  object T4 extends Function4[String, Int, Long, List[Int], T4] with ShowAndEq[T4] {
    implicit val a: scalaprops.Gen[T4] = scalaprops.Gen[(String, Int, Long, List[Int])].map(T4.tupled)
  }
  final case class T5(_1: String, _2: Int, _3: Long, _4: List[Int], _5: (Int, Byte))
      extends Product5[String, Int, Long, List[Int], (Int, Byte)]
  object T5 extends Function5[String, Int, Long, List[Int], (Int, Byte), T5] with ShowAndEq[T5] {
    implicit val a: scalaprops.Gen[T5] = scalaprops.Gen[(String, Int, Long, List[Int], (Int, Byte))].map(T5.tupled)
  }

  val unzip3CaseClass = forAll { a: IArray[T3] =>
    val (_1, _2, _3) = a.unzip3
    (_1.toList, _2.toList, _3.toList) must_=== a.toList.map(x => (x._1, x._2, x._3)).unzip3
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
  }

  val unzip3Tuple = forAll { a: IArray[(Int, String, Long)] =>
    val (_1, _2, _3) = a.unzip3
    (_1.toList, _2.toList, _3.toList) must_=== a.toList.unzip3
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
  }

  val unzip4CaseClass = forAll { a: IArray[T4] =>
    val (_1, _2, _3, _4) = a.unzip4
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
    _4 must_=== a.map(_._4)
  }

  val unzip4Tuple = forAll { a: IArray[(Int, String, Long, List[Int])] =>
    val (_1, _2, _3, _4) = a.unzip4
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
    _4 must_=== a.map(_._4)
  }

  val unzip5 = forAll { a: IArray[T5] =>
    val (_1, _2, _3, _4, _5) = a.unzip5
    _1 must_=== a.map(_._1)
    _2 must_=== a.map(_._2)
    _3 must_=== a.map(_._3)
    _4 must_=== a.map(_._4)
    _5 must_=== a.map(_._5)
  }

  val zipWithIndex = forAll { a: IArray[Int] =>
    a.zipWithIndex.toList must_=== a.toList.zipWithIndex
  }

  val flatten = forAll { a: IArray[IArray[Maybe[Int]]] =>
    a.flatten.toList must_=== a.map(_.toList).toList.flatten
  }.mapSize(_ / 4)

  val exists = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.exists(f) must_=== a.toList.exists(f)
  }

  val `toArray Int` = forAll { a: IArray[Int] =>
    a.toArray.toList must_=== a.toList
  }

  val to = forAll { a: IArray[Int] =>
    a.to[Vector].toList must_=== a.toList
  }

  val `toArray String` = forAll { a: IArray[String] =>
    a.toArray.toList must_=== a.toList
  }

  val toIterator = forAll { a: IArray[String] =>
    a.toIterator.toList must_=== a.toList
  }

  val filter = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.filter(f).toList must_=== a.toList.filter(f)
  }

  val collectBy = forAll { a: IArray[Int \/ String] =>
    implicit val (s, e) = (Show.showA[\/-[String]], Equal.equalA[\/-[String]])
    a.collectBy[\/-[String]] must_=== a.collect { case r @ \/-(_) => r }
  }

  val collect = forAll { a: IArray[Int] =>
    var sideEffect = 0
    val f: PartialFunction[Int, String] = { case i if { sideEffect += 1; i % 2 == 0 } => (i * 3).toString }
    val x = a.collect(f).toList
    sideEffect must_=== a.length
    x must_=== a.toList.collect(f)
  }

  val `collect String` = forAll { (aa: IArray[Int], f: PartialFunction[Int, Int]) =>
    aa.collect(f).toList must_=== aa.toList.collect(f)
  }

  val `collectFirst collectLast` = forAll { (a: IArray[Int], f: PartialFunction[Int, Byte]) =>
    a.collectFirst(f) must_=== a.toList.collectFirst(f)
    a.collectLast(f) must_=== a.reverse.collectFirst(f)
  }

  val find = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.find(f) must_=== a.toList.find(f)
  }

  val findRight = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.findRight(f) must_=== a.reverse.find(f)
  }

  val flatMap = forAll { (a: IArray[Int], f: Int => IArray[Int]) =>
    a.flatMap(f).toList must_=== a.toList.flatMap(x => f(x).toList)
  }.mapSize(_ / 2)

  val map = forAll { (a: IArray[Int], f: Int => String) =>
    a.map(f).toList must_=== a.toList.map(f)
  }

  val mapTo = forAll { (a: IArray[Int], f: Int => String) =>
    val list: List[String] = a.mapTo(f)
    val vector: Vector[String] = a.mapTo(f)
    list must_=== a.to[List].map(f)
    vector must_=== a.map(f).to[Vector]
  }

  val foreach = forAll { a: IArray[Int] =>
    val buf1, buf2 = new scala.collection.mutable.ListBuffer[Int]
    val f1 = buf1 += (_: Int)
    val f2 = buf2 += (_: Int)
    a.foreach(f1)
    a.toList.foreach(f2)
    buf1.toList must_=== buf2.toList
  }

  val `cojoin cobind` = forAll { (a: IArray[Int], f: List[Int] => Int) =>
    a.cojoin must_=== a.cobind(x => x)
    a.cojoin.map(_.toList).toList must_=== Cobind[List].cojoin(a.toList)
    a.cobind(x => f(x.toList)).toList must_=== Cobind[List].cobind(a.toList)(f)
  }.mapSize(_ / 4)

  val `reverse.reverse` = forAll { a: IArray[Int] =>
    a.reverse.reverse must_=== a
  }

  val reverse = forAll { a: IArray[Int] =>
    a.reverse.toList must_=== a.toList.reverse
  }

  val reverse_::: = forAll { (a: IArray[Int], b: IArray[Int]) =>
    (a reverse_::: b).toList must_=== (a.toList reverse_::: b.toList)
  }

  val take = forAll { (a: IArray[Int], n: Int) =>
    a.takeL(n).toList must_=== a.toList.take(n)
  }

  val takeWhileL = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.takeWhileL(f).toList must_=== a.toList.takeWhile(f)
  }

  val takeWhileR = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.takeWhileR(f).toList must_=== a.toList.reverse.takeWhile(f).reverse
  }

  val takeR = forAll { (a: IArray[Int], n: Int) =>
    a.takeR(n).toList must_=== a.toList.takeRight(n)
  }

  val count = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.count(f) must_=== a.toList.count(f)
  }

  val dropL = forAll { (a: IArray[Int], n: Int) =>
    a.dropL(n).toList must_=== a.toList.drop(n)
  }

  val dropWhileL = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.dropWhileL(f).toList must_=== a.toList.dropWhile(f)
  }

  val dropR = forAll { (a: IArray[Int], n: Int) =>
    a.dropR(n).toList must_=== a.toList.dropRight(n)
  }

  val contains = forAll { (a: IArray[Int], n: Int) =>
    a.contains(n) must_=== a.toList.contains(n)
  }

  val splitAt = forAll { (a: IArray[Int], n: Int) =>
    val (x1, x2) = a.splitAt(n)
    val (y1, y2) = a.toList.splitAt(n)
    (x1.toList must_=== y1) && (x2.toList must_=== y2)
  }

  val ++ = forAll { (a: IArray[Int], b: IArray[Int]) =>
    (a ++ b).toList must_=== (a.toList ++ b.toList)
  }

  val `+: Int` = forAll { (a: IArray[Int], b: Int) =>
    (b +: a).toList must_=== (b +: a.toList)
  }

  val `+: String` = forAll { (a: IArray[String], b: String) =>
    (b +: a).toList must_=== (b +: a.toList)
  }

  val `:+ Int` = forAll { (a: IArray[Int], b: Int) =>
    (a :+ b).toList must_=== (a.toList :+ b)
  }

  val `:+ String` = forAll { (a: IArray[String], b: String) =>
    (a :+ b).toList must_=== (a.toList :+ b)
  }

  val forall = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.forall(f) must_=== a.toList.forall(f)
  }

  val dropWhileR = forAll { (a: IArray[Int], f: Int => Boolean) =>
    a.dropWhileR(f).toList must_=== a.toList.reverse.dropWhile(f).reverse
  }

  val span = forAll { (a: IArray[Int], f: Int => Boolean) =>
    val (x1, x2) = a.span(f)
    val (y1, y2) = a.toList.span(f)
    (x1.toList must_=== y1) && (x2.toList must_=== y2)
  }

  val partition = forAll { (a: IArray[Int], f: Int => Boolean) =>
    val (x1, x2) = a.partition(f)
    val (y1, y2) = a.toList.partition(f)
    (x1.toList must_=== y1) && (x2.toList must_=== y2)
  }

  val updated = forAll { (a: IArray[Int], index: Int, elem: Int) =>
    if (0 <= index && index < a.length)
      a.updated(index, elem).toList must_=== a.toList.updated(index, elem)
    else
      a.updated(index, elem).mustThrowA[iarray.Platform.IndexOutOfBoundsError]
  }

  val indexOfL = forAll { (a: IArray[Byte], z: Byte) =>
    a.indexOfL(z) must_=== Option(a.toList.indexOf(z)).filter(_ >= 0)
  }

  val indexOfR = forAll { (a: IArray[Byte], z: Byte) =>
    a.indexOfR(z) must_=== Option(a.toList.lastIndexOf(z)).filter(_ >= 0)
  }

  val `sorted Int` = forAll { a: IArray[Int] =>
    a.sorted.toList must_=== a.toList.sorted
  }

  val `sorted String` = forAll { a: IArray[String] =>
    a.sorted.toList must_=== a.toList.sorted
  }

  val sortBy = forAll { a: IArray[Int] =>
    a.sortBy(-_).toList must_=== a.toList.sortBy(-_)
    a.sortBy(_.toHexString).toList must_=== a.toList.sortBy(_.toHexString)
  }

  val sortWith = forAll { a: IArray[Int] =>
    a.sortWith(_ > _).toList must_=== a.toList.sortWith(_ > _)
  }

  val from = forAll { a: List[Int] =>
    IArray.from(a).toList must_=== a
    IArray.fromList(a).toList must_=== a
    IArray.from(a.toVector).toList must_=== a
    IArray.fromIndexedSeq(a.toVector).toList must_=== a
  }

  val reverseMap = forAll { (a: IArray[Int], f1: Int => (Int, Long), f2: Int => Byte) =>
    a.reverseMap(f1).toList must_=== a.toList.reverseMap(f1)
    a.reverseMap(f2).toList must_=== a.toList.reverseMap(f2)
  }

  val slice = forAll { (a: IArray[Int], from: Byte, until: Byte) =>
    a.slice(from, until).toList must_=== a.toList.slice(from, until)
  }

  val `mapAccumL mapAccumR` = forAll { a: IArray[List[Int]] =>
    import syntax.bifunctor._
    val f = (acc: List[Int], x: List[Int]) => (x.sum :: x ::: acc, acc.sum)
    val F = Traverse[List]
    a.mapAccumL(List[Int]())(f).rightMap(_.toList) must_=== F.mapAccumL(a.toList, List[Int]())(f)
    a.mapAccumR(List[Int]())(f).rightMap(_.toList) must_=== F.mapAccumR(a.toList, List[Int]())(f)
  }.mapSize(_ / 4)

  val fold = forAll { a: IArray[List[Int]] =>
    Foldable[IArray].fold(a) must_=== Foldable[List].fold(a.toList)
  }.mapSize(_ / 4)

  val fold1Opt = forAll { a: IArray[NonEmptyList[Int]] =>
    a.fold1Opt must_=== Foldable[List].foldMap1Opt(a.toList)(conform)
  }.mapSize(_ / 4)

  val `foldl foldl1` = forAll { (a: IArray[Int], z: Int) =>
    a.foldl(z)(_ - _) must_=== a.toList.foldLeft(z)(_ - _)
    a.foldl1(_ - _) must_=== a.toList.reduceLeftOption(_ - _)
  }

  val `foldr foldr1` = forAll { (a: IArray[Int], z: List[Int]) =>
    a.foldr(z)(_ :: _) must_=== a.toList.foldRight(z)(_ :: _)
    a.foldr1(_ - _) must_=== a.toList.reduceRightOption(_ - _)
  }

  val `no stack overflow foldLeftM, foldRightM` = forAll {
    val n = 100000
    Foldable[IArray].foldLeftM[Id.Id, Int, Int](IArray.fillAll(n)(1), 0)(_ + _) must_=== n
    Foldable[IArray].foldRightM[Id.Id, Int, Int](IArray.fillAll(n)(1), 0)(_ + _) must_=== n
  }

  val foldLeftM = forAll { (a: IArray[Int], z: Vector[Int]) =>
    Foldable[IArray].foldLeftM[Id.Id, Int, Vector[Int]](a, z)(_ :+ _) must_=== a.foldl(z)(_ :+ _)
    Foldable[IArray].foldLeftM(a, Vector.empty[Int])((a, b) => Option(a :+ b)) must_=== Option(a.to[Vector])
  }

  val foldRightM = forAll { (a: IArray[Int], z: List[Int]) =>
    Foldable[IArray].foldRightM[Id.Id, Int, List[Int]](a, z)(_ :: _) must_=== a.foldr(z)(_ :: _)
    Foldable[IArray].foldRightM(a, List.empty[Int])((a, b) => Option(a :: b)) must_=== Option(a.toList)
  }

  val foldMapL1 = forAll { a: IArray[Int] =>
    val z = (a: Int) => Vector(a)
    a.foldMapL1(z)(_ :+ _) must_=== {
      if (a.isEmpty) None
      else Some(a.tailOption.get.toList.foldLeft(z(a.headOption.get))(_ :+ _))
    }
  }

  val foldMapR1Opt = forAll { a: IArray[Int] =>
    val z = (a: Int) => List(a)
    a.foldMapR1Opt(z)(_ :: _) must_=== {
      if (a.isEmpty) None
      else Some(a.initOption.get.toList.foldRight(z(a.lastOption.get))(_ :: _))
    }
  }

  val scanLeft = forAll { (a: IArray[Int], z: Int, f: (Int, Int) => Int) =>
    a.scanLeft(z)(f).toList must_=== a.toList.scanLeft(z)(f)
  }

  val scanRight = forAll { (a: IArray[Int], z: List[Int]) =>
    a.scanRight(z)(_ :: _).toList must_=== a.toList.scanRight(z)(_ :: _)
  }

  val scanLeft1 = forAll { a: IArray[Int] =>
    a.scanLeft1(_ - _).toList must_=== (
      if (a.isEmpty) List()
      else a.toList.tail.scanLeft(a.headOption.get)(_ - _)
    )
  }

  val scanRight1 = forAll { a: IArray[String] =>
    a.scanRight1(_ + _).toList must_=== (
      if (a.isEmpty) List()
      else a.toList.init.scanRight(a.lastOption.get)(_ + _)
    )
  }

  val foldMap1Opt = forAll { a: IArray[Int] =>
    val f = (x: Int) => Vector(x)
    a.foldMap1Opt(f) must_=== Foldable[List].foldMap1Opt(a.toList)(f).map(_.toVector)
  }

  val tailOption = forAll { a: IArray[Int] =>
    a.tailOption.map(_.toList) must_=== (
      if (a.isEmpty) None
      else Some(a.toList.tail)
    )
  }

  val tailMaybe = forAll { a: IArray[Int] =>
    a.tailMaybe.toOption must_=== a.tailOption
  }

  val headOption = forAll { a: IArray[Int] =>
    a.headOption must_=== a.toList.headOption
  }

  val headMaybe = forAll { a: IArray[Int] =>
    a.headMaybe must_=== a.toIList.headMaybe
  }

  val lastOption = forAll { a: IArray[Int] =>
    a.lastOption must_=== a.toList.lastOption
  }

  val lastMaybe = forAll { a: IArray[Int] =>
    a.lastMaybe must_=== Maybe.fromOption(a.lastOption)
  }

  val initOption = forAll { a: IArray[Int] =>
    a.initOption.map(_.toList) must_=== (
      if (a.isEmpty) None
      else Some(a.toList.init)
    )
  }

  val initMaybe = forAll { a: IArray[Int] =>
    a.initMaybe.toOption must_=== a.initOption
  }

  val sum = forAll { a: IArray[Int] =>
    a.sum must_=== a.toIterator.sum
  }

  val maxOf = forAll { a: IArray[Int] =>
    a.maxOf(_.toString) must_=== a.map(_.toString).maxBy(conform)
  }

  val minOf = forAll { a: IArray[Int] =>
    a.minOf(_.toString) must_=== a.map(_.toString).minBy(conform)
  }

  val maxBy = forAll { a: IArray[Int] =>
    a.maxBy(_.toString) must_=== (
      if (a.isEmpty) None
      else Some(a.toList.maxBy(_.toString))
    )
  }

  val minBy = forAll { a: IArray[Int] =>
    a.minBy(_.toString) must_=== (
      if (a.isEmpty) None
      else Some(a.toList.minBy(_.toString))
    )
  }

  val max = forAll { a: IArray[Int] =>
    if (a.isEmpty) {
      a.max must_=== None
    } else {
      a.max must_=== Some(a.toList.max)
    }
  }

  val min = forAll { a: IArray[Int] =>
    if (a.isEmpty) {
      a.min must_=== None
    } else {
      a.min must_=== Some(a.toList.min)
    }
  }

  val interleave = forAll { (xs: IArray[Int], ys: IArray[Int]) =>
    import std.stream._
    val z = xs.interleave(ys)
    z.length must_=== (xs.length + ys.length)
    std.stream.interleave(xs.to[Stream], ys.to[Stream]) must_=== z.to[Stream]
  }

  val intersperse = forAll { xs: IArray[String @@ AlphaNum] =>
    import syntax.std.list._
    xs.intersperse(Tag(",")).toList must_=== xs.toList.intersperse(Tag(","))
  }

  val intercalate = forAll { xs0: IArray[String @@ AlphaNum] =>
    val xs: IArray[String] = Tag.unsubst(xs0)
    xs.intercalate(" , ") must_=== Foldable[List].intercalate(xs.toList, " , ")
    xs.intercalate(",") must_=== xs.mkString(",")
    xs.intercalate(",") must_=== xs.intercalate1Opt(",").getOrElse(Monoid[String].zero)
  }

  val `test toString` = forAll { xs: IArray[Int] =>
    xs.toString must_=== xs.toList.mkString("IArray(", ", ", ")")
  }

  val mkString = forAll { (xs: IArray[Int], a: String, b: String, c: String) =>
    xs.mkString(a, b, c) must_=== xs.toArray.mkString(a, b, c)
  }.mapSize(_ / 4)

  val startsWith = forAll { (a: IArray[Int], b: IArray[Int], n: Int) =>
    if (n >= 0) {
      a.startsWith(b, n) must_=== a.toList.startsWith(b.toList, n)
    } else {
      a.startsWith(b, n).mustThrowA[IllegalArgumentException]
    }
  }

  val endsWith = forAll { (a: IArray[Int], b: IArray[Int]) =>
    a.endsWith(b) must_=== a.toList.endsWith(b.toList)
  }

  val `for comprehension` = forAll { (xs: IArray[Int], ys: IArray[String]) =>
    val a = for { x <- xs; if x % 2 == 0; y <- ys } yield (x, y)
    val b = for { x <- xs.toList; if x % 2 == 0; y <- ys.toList } yield (x, y)

    a must_=== b.to[IArray]

    val buf1, buf2 = List.newBuilder[(Int, String)]

    for {
      x <- xs.toList; if x % 2 == 0; y <- ys.toList; if true
    } { buf1 += ((x, y)) }

    for {
      x <- xs; if x % 2 == 0; y <- ys; if true
    } { buf2 += ((x, y)) }

    buf1.result must_=== buf2.result
  }

  val `separate Eithers, Validations` = forAll { eithers: IArray[Int \/ String] =>
    val F = Bifunctor[Tuple2]
    import F.bifunctorSyntax._
    MonadPlus[IArray].separate(eithers).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(eithers.toList)
    val validations = eithers.map(_.validation)
    MonadPlus[IArray].separate(validations).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(
      validations.toList)
    val stdEithers = eithers.map(_.toEither)
    MonadPlus[IArray].separate(stdEithers).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(
      stdEithers.toList)
  }

  val `partitionTry` = forAll { xs: IArray[scala.util.Try[Int]] =>
    import std.java.throwable._
    implicit val throwableEq = Equal.equalRef[Throwable]
    IArray.partitionTry(xs) must_=== IArray.partitionEithers(xs.map(tryEitherIso.from(_)))
  }

  val `separate Tuple2, LazyTuple2` = forAll { tuples: IArray[(Int, String)] =>
    val F = Bifunctor[Tuple2]
    import F.bifunctorSyntax._
    MonadPlus[IArray].separate(tuples).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(tuples.toList)
    val lazyTuples = tuples.map { case (a, b) => LazyTuple2(a, b) }
    MonadPlus[IArray].separate(lazyTuples).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(
      lazyTuples.toList)
  }

  val `separate These` = forAll { these: IArray[Int \&/ String] =>
    val F = Bifunctor[Tuple2]
    import F.bifunctorSyntax._
    MonadPlus[IArray].separate(these).bimap(_.toList, _.toList) must_=== MonadPlus[List].separate(these.toList)
  }

  val partitionLazyTuples = forAll {
    IArray.partitionLazyTuples(IArray(LazyTuple2(1, sys.error("error"): String)))._1 must_=== IArray(1)
    IArray.partitionLazyTuples(IArray(LazyTuple2(sys.error("error"): Int, "a")))._2 must_=== IArray("a")
  }

  val partitionLazyTuple3 = forAll {
    def e: Int = sys.error("error")
    IArray.partitionLazyTuple3(IArray(LazyTuple3(1, e, e)))._1 must_=== IArray(1)
    IArray.partitionLazyTuple3(IArray(LazyTuple3(e, 1, e)))._2 must_=== IArray(1)
    IArray.partitionLazyTuple3(IArray(LazyTuple3(e, e, 1)))._3 must_=== IArray(1)
  }

  val groupBy1 = forAll { (xs: IArray[Int], x: Int) =>
    // https://github.com/scala-native/scala-native/issues/1078
    val f = (a: Int) => if (x == 0 || a == Int.MinValue) a else a % x
    xs.groupBy1(f).map(_.toNel) must_=== ==>>.fromList(std.list.groupBy1(xs.toList)(f).toList)
  }

  val traverseS = forAll { (xs: IArray[Int], n: Int, z: Int) =>
    val F = Traverse[List]
    import F.traverseSyntax._
    val f = (a: Int) => State.gets[Int, Int](b => if (b % 2 == 0) b - a + n else a - b)
    val x = Traverse[IArray].traverseS(xs)(f)(z)
    x must_=== Bifunctor[Tuple2].rightMap(xs.toList.traverseS(f)(z))(_.to[IArray])

    val G = new Traverse[IArray] {
      def traverseImpl[G[_]: Applicative, A, B](fa: iarray.IArray[A])(f: A => G[B]) =
        Traverse[IArray].traverseImpl(fa)(f)
    }

    x must_=== G.traverseS(xs)(f)(z)
  }

  val `tailOptionEodo initOptionEndo` = Property.forAllG(
    Gen[IArray[Int]],
    Gen.choose(-10, 10),
    Gen.choose(-10, 10)
  ) { (xs, a, b) =>
    import syntax.monoid._
    val i = IArray.initOptionEndo[Int].multiply(b)
    val t = IArray.tailOptionEndo[Int].multiply(a)

    (i |+| t).run(xs) match {
      case Some(x) =>
        x must_=== xs.dropR(b).dropL(a)
      case None =>
        xs.length < ((a max 0) + (b max 0))
    }
  }

  val zipperEnd = forAll { xs: IArray[String @@ AlphaNum] =>
    import syntax.std.list._
    xs.zipperEnd must_=== xs.toList.zipperEnd
  }

}
