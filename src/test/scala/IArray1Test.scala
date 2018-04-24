package iarray

import scalaprops.Property.forAll
import scalaprops.ScalapropsScalaz._
import scalaz._
import std.anyVal._, std.list._, std.option._, std.string._, std.tuple._, std.vector._
import IArray.conform

object IArray1Test extends TestCommon {

  val partition = forAll { (a: IArray1[Int], f: Int => Boolean) =>
    a.partition(f) must_=== a.toIArray.partition(f)
  }

  val dropL = forAll { (a: IArray1[Int], n: Int) =>
    a.dropL(n).toList must_=== a.toList.drop(n)
  }

  val toIArray = forAll { a: IArray1[Int] =>
    a.toIArray must_=== IArray.fromList(a.toList)
  }

  val collect = forAll { (a: IArray1[Int], f: PartialFunction[Int, String]) =>
    a.collect(f).toList must_=== a.toList.collect(f)
  }

  val unite1 = forAll { a: IArray1[NonEmptyList[Int]] =>
    a.unite1.toNel must_=== Bind[NonEmptyList].join(a.toNel)
  }.mapSize(_ / 4)

  val unite = forAll { a: IArray1[List[Int]] =>
    a.unite must_=== IArray.fromList(a.toList.flatten)
  }.mapSize(_ / 4)

  val fromOneAnd = forAll { a: OneAnd[Vector, Int] =>
    val x = IArray1.fromOneAnd(a)
    x.head must_=== a.head
    x.tail.toVector must_=== a.tail
  }

  val maxOf = forAll { (a: IArray1[Int], f: Int => Int) =>
    a.maxOf(f) must_=== a.toList.map(f).max
  }

  val minOf = forAll { (a: IArray1[Int], f: Int => Int) =>
    a.minOf(f) must_=== a.toList.map(f).min
  }

  val maxBy = forAll { (a: IArray1[Int], f: Int => Int) =>
    a.maxBy(conform) must_=== a.toList.maxBy(conform)
    a.maxBy(f) must_=== a.toList.maxBy(f)
  }

  val minBy = forAll { (a: IArray1[Int], f: Int => Int) =>
    a.minBy(conform) must_=== a.toList.minBy(conform)
    a.minBy(f) must_=== a.toList.minBy(f)
  }

  val foldl = forAll { (a: IArray1[Int], f: (List[Int], Int) => List[Int]) =>
    a.foldl(List[Int]())(f) must_=== a.toList.foldLeft(List[Int]())(f)
  }.mapSize(_ / 4)

  val foldl1 = forAll { (a: IArray1[List[Int]], f: (List[Int], List[Int]) => List[Int]) =>
    a.foldl1(f) must_=== a.toList.reduceLeft(f)
  }.mapSize(_ / 4)

  val zipAll = forAll { (as: IArray1[Int], bs: IArray1[Long], a: Int, b: Long) =>
    as.zipAll(bs, a, b).toList must_=== as.toList.zipAll(bs.toList, a, b)
  }

  val zipWithIndex = forAll { a: IArray1[Int] =>
    a.zipWithIndex.toList must_=== a.toList.zipWithIndex
  }

  val `collectFirst collectLast` = forAll { (a: IArray1[Int], f: PartialFunction[Int, String]) =>
    a.collectFirst(f) must_=== a.toList.collectFirst(f)
    a.collectLast(f) must_=== a.toList.reverse.collectFirst(f)
  }

  val merge = forAll { (a: IArray1[Int], b: IArray1[Int]) =>
    Align[IArray1].merge(a, b).toList must_=== Align[List].merge(a.toList, b.toList)
  }

  val `cojoin cobind` = forAll { a: IArray1[Int] =>
    a.cojoin must_=== a.cobind(conform)
    a.cojoin.map(_.toNel).toNel must_=== Comonad[NonEmptyList].cojoin(a.toNel)
  }

  val sorted = forAll { (a: IArray1[Int], b: IArray[String]) =>
    a.sorted.toList must_=== a.toList.sorted
    b.sorted.toList must_=== b.toList.sorted
  }

  val sortBy = forAll { a: IArray1[Int] =>
    a.sortBy(-_).toList must_=== a.toList.sortBy(-_)
    val f = (_: Int).toString.reverse
    a.sortBy(f).toList must_=== a.toList.sortBy(f)
  }

  val sortWith = forAll { a: IArray1[Int] =>
    a.sortWith(_ > _).toList must_=== a.toList.sortWith(_ > _)
  }

  val `max min` = forAll { a: IArray1[Int] =>
    import syntax.foldable1._
    a.max must_=== a.toNel.maximum1
    a.min must_=== a.toNel.minimum1
  }

  val `toIterator` = forAll { a: IArray1[Int] =>
    a.toIterator.toList must_=== a.toList
  }

  val `toArray Int` = forAll { (a: Int, as: Array[Int]) =>
    IArray1(a, IArray.from(as)).toArray.toList must_=== (a +: as).toList
  }

  val `toArray String` = forAll { (a: String, as: Array[String]) =>
    IArray1(a, IArray.from(as)).toArray.toList must_=== (a +: as).toList
  }

  val `+: :+` = forAll { (a: Int, as: IArray1[Int]) =>
    (a +: as).toList must_=== a +: as.toList
    (as :+ a).toList must_=== as.toList :+ a
  }

  val init = forAll { as: IArray1[Int] =>
    as.init must_=== as.toIArray.initOption.get
  }

  val forall = forAll { (as: IArray1[Int], a: Int) =>
    as.forall(_ > a) must_=== as.toList.forall(_ > a)
  }

  val exists = forAll { (as: IArray1[Int], a: Int) =>
    as.exists(_ > a) must_=== as.toList.exists(_ > a)
  }

  val find = forAll { (as: IArray1[Int], f: Int => Boolean) =>
    as.find(f) must_=== as.toList.find(f)
  }

  val findRight = forAll { (as: IArray1[Int], f: Int => Boolean) =>
    as.findRight(f) must_=== as.reverse.find(f)
  }

  val contains = forAll { (as: IArray1[Int], a: Int) =>
    as.contains(a) must_=== as.toList.contains(a)
  }

  val reverse = forAll { as: IArray1[Int] =>
    as.reverse.toNel must_=== as.toNel.reverse
  }

  val reverseMap = forAll { (as: IArray1[Int], f: Int => Byte) =>
    as.reverseMap(f).toList must_=== as.toList.reverseMap(f)
  }

  val flatten = forAll { as: IArray1[IArray1[Int]] =>
    as.flatten.toList must_=== as.toList.flatMap(_.toList)
  }.mapSize(_ / 4)

  val `indexOfL indexOfR` = forAll { (as: IArray1[Int], a: Int) =>
    def toOpt(n: Int) = if (n < 0) None else Some(n)
    as.indexOfL(a) must_=== toOpt(as.toList.indexOf(a))
    as.indexOfR(a) must_=== toOpt(as.toList.lastIndexOf(a))
  }

  val intersperse = forAll { xs: IArray1[String] =>
    import syntax.std.list._
    xs.intersperse(",").toList must_=== xs.toList.intersperse(",")
  }

  val intercalate1 = forAll { xs: IArray1[String] =>
    xs.intercalate1(",") must_=== Foldable[NonEmptyList].intercalate(xs.toNel, ",")
  }

  val `equals hashCode law` = forAll { (a1: IArray1[Int], a2: IArray1[Int], a3: IArray1[Int]) =>
    (a1 == a2) must_=== (a2 == a1)
    (a1 == a1.map(conform)) must_=== true
    std.boolean.conditional((a1 == a2) && (a2 == a3), a1 == a3) must_=== true
    std.boolean.conditional(a1 == a2, a1.## == a2.##) must_=== true
  }

  val `fromNel toNel` = forAll { xs: NonEmptyList[Int] =>
    IArray1.fromNel(xs).toNel must_=== xs
  }

  val scanLeft = forAll { (xs: IArray1[Int], n: Int, z: List[Int]) =>
    val f = (a: List[Int], b: Int) => (n + b) :: a
    xs.scanLeft(z)(f).toList must_=== xs.toList.scanLeft(z)(f)
  }

  val scanRight = forAll { (xs: IArray1[Int], n: Int, z: List[Int]) =>
    val f = (a: Int, b: List[Int]) => (n + a) :: b
    xs.scanRight(z)(f).toList must_=== xs.toList.scanRight(z)(f)
    xs.scanRight(z)(f) must_=== xs.reverse.scanLeft(z)((a, b) => f(b, a)).reverse
  }

}
