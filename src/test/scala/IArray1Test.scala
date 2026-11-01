package iarray

import scalaprops.Property.forAll
import scalaprops.ScalapropsScalaz._
import scalaz._
import std.anyVal._, std.list._, std.option._, std.string._, std.tuple._, std.vector._
import IArray.conform

object IArray1Test extends TestCommon {
  property("partition") = forAll { (a: IArray1[Int], f: Int => Boolean) =>
    a.partition(f) must_=== a.toIArray.partition(f)
  }

  property("dropL") = forAll { (a: IArray1[Int], n: Int) => a.dropL(n).toList must_=== a.toList.drop(n) }

  property("toIArray") = forAll { (a: IArray1[Int]) => a.toIArray must_=== IArray.fromList(a.toList) }

  property("collect") = forAll { (a: IArray1[Int], f: PartialFunction[Int, String]) =>
    a.collect(f).toList must_=== a.toList.collect(f)
  }

  property("unite1") = forAll { (a: IArray1[NonEmptyList[Int]]) =>
    a.unite1.toNel must_=== Bind[NonEmptyList].join(a.toNel)
  }.mapSize(_ / 4)

  property("unite") =
    forAll { (a: IArray1[List[Int]]) => a.unite must_=== IArray.fromList(a.toList.flatten) }.mapSize(_ / 4)

  property("fromOneAnd") = forAll { (a: OneAnd[Vector, Int]) =>
    val x = IArray1.fromOneAnd(a)
    x.head must_=== a.head
    x.tail.toVector must_=== a.tail
  }

  property("maxOf") = forAll { (a: IArray1[Int], f: Int => Int) => a.maxOf(f) must_=== a.toList.map(f).max }

  property("minOf") = forAll { (a: IArray1[Int], f: Int => Int) => a.minOf(f) must_=== a.toList.map(f).min }

  property("maxBy") = forAll { (a: IArray1[Int], f: Int => Int) =>
    a.maxBy(conform) must_=== a.toList.maxBy(conform)
    a.maxBy(f) must_=== a.toList.maxBy(f)
  }

  property("minBy") = forAll { (a: IArray1[Int], f: Int => Int) =>
    a.minBy(conform) must_=== a.toList.minBy(conform)
    a.minBy(f) must_=== a.toList.minBy(f)
  }

  property("foldl") = forAll { (a: IArray1[Int], f: (List[Int], Int) => List[Int]) =>
    a.foldl(List[Int]())(f) must_=== a.toList.foldLeft(List[Int]())(f)
  }.mapSize(_ / 4)

  property("foldl1") = forAll { (a: IArray1[List[Int]], f: (List[Int], List[Int]) => List[Int]) =>
    a.foldl1(f) must_=== a.toList.reduceLeft(f)
  }.mapSize(_ / 4)

  property("zipAll") = forAll { (as: IArray1[Int], bs: IArray1[Long], a: Int, b: Long) =>
    as.zipAll(bs, a, b).toList must_=== as.toList.zipAll(bs.toList, a, b)
  }

  property("zipWithIndex") = forAll { (a: IArray1[Int]) => a.zipWithIndex.toList must_=== a.toList.zipWithIndex }

  property("collectFirst collectLast") = forAll { (a: IArray1[Int], f: PartialFunction[Int, String]) =>
    a.collectFirst(f) must_=== a.toList.collectFirst(f)
    a.collectLast(f) must_=== a.toList.reverse.collectFirst(f)
  }

  property("merge") = forAll { (a: IArray1[Int], b: IArray1[Int]) =>
    Align[IArray1].merge(a, b).toList must_=== Align[List].merge(a.toList, b.toList)
  }

  property("cojoin cobind") = forAll { (a: IArray1[Int]) =>
    a.cojoin must_=== a.cobind(conform)
    a.cojoin.map(_.toNel).toNel must_=== Comonad[NonEmptyList].cojoin(a.toNel)
  }

  property("sorted") = forAll { (a: IArray1[Int], b: IArray[String]) =>
    a.sorted.toList must_=== a.toList.sorted
    b.sorted.toList must_=== b.toList.sorted
  }

  property("sortBy") = forAll { (a: IArray1[Int]) =>
    a.sortBy(-_).toList must_=== a.toList.sortBy(-_)
    val f = (_: Int).toString.reverse
    a.sortBy(f).toList must_=== a.toList.sortBy(f)
  }

  property("sortWith") = forAll { (a: IArray1[Int]) => a.sortWith(_ > _).toList must_=== a.toList.sortWith(_ > _) }

  property("max min") = forAll { (a: IArray1[Int]) =>
    import syntax.foldable1._
    a.max must_=== a.toNel.maximum1
    a.min must_=== a.toNel.minimum1
  }

  property("toIterator") = forAll { (a: IArray1[Int]) => a.toIterator.toList must_=== a.toList }

  property("toArray Int") = forAll { (a: Int, as: Array[Int]) =>
    IArray1(a, IArray.from(as)).toArray.toList must_=== (a +: as).toList
  }

  property("toArray String") = forAll { (a: String, as: Array[String]) =>
    IArray1(a, IArray.from(as)).toArray.toList must_=== (a +: as).toList
  }

  property("+: :+") = forAll { (a: Int, as: IArray1[Int]) =>
    (a +: as).toList must_=== a +: as.toList
    (as :+ a).toList must_=== as.toList :+ a
  }

  property("init") = forAll { (as: IArray1[Int]) => as.init must_=== as.toIArray.initOption.get }

  property("forall") = forAll { (as: IArray1[Int], a: Int) => as.forall(_ > a) must_=== as.toList.forall(_ > a) }

  property("exists") = forAll { (as: IArray1[Int], a: Int) => as.exists(_ > a) must_=== as.toList.exists(_ > a) }

  property("find") = forAll { (as: IArray1[Int], f: Int => Boolean) => as.find(f) must_=== as.toList.find(f) }

  property("findRight") = forAll { (as: IArray1[Int], f: Int => Boolean) =>
    as.findRight(f) must_=== as.reverse.find(f)
  }

  property("contains") = forAll { (as: IArray1[Int], a: Int) => as.contains(a) must_=== as.toList.contains(a) }

  property("reverse") = forAll { (as: IArray1[Int]) => as.reverse.toNel must_=== as.toNel.reverse }

  property("reverseMap") = forAll { (as: IArray1[Int], f: Int => Byte) =>
    as.reverseMap(f).toList must_=== as.toList.reverseMap(f)
  }

  property("flatten") =
    forAll { (as: IArray1[IArray1[Int]]) => as.flatten.toList must_=== as.toList.flatMap(_.toList) }.mapSize(_ / 4)

  property("indexOfL indexOfR") = forAll { (as: IArray1[Int], a: Int) =>
    def toOpt(n: Int) = if (n < 0) None else Some(n)
    as.indexOfL(a) must_=== toOpt(as.toList.indexOf(a))
    as.indexOfR(a) must_=== toOpt(as.toList.lastIndexOf(a))
  }

  property("intersperse") = forAll { (xs: IArray1[String]) =>
    import syntax.std.list._
    xs.intersperse(",").toList must_=== xs.toList.intersperse(",")
  }

  property("intercalate1") = forAll { (xs: IArray1[String]) =>
    xs.intercalate1(",") must_=== Foldable[NonEmptyList].intercalate(xs.toNel, ",")
  }

  property("equals hashCode law") = forAll { (a1: IArray1[Int], a2: IArray1[Int], a3: IArray1[Int]) =>
    (a1 == a2) must_=== (a2 == a1)
    (a1 == a1.map(conform)) must_=== true
    std.boolean.conditional((a1 == a2) && (a2 == a3), a1 == a3) must_=== true
    std.boolean.conditional(a1 == a2, a1.## == a2.##) must_=== true
  }

  property("fromNel toNel") = forAll { (xs: NonEmptyList[Int]) => IArray1.fromNel(xs).toNel must_=== xs }

  property("scanLeft") = forAll { (xs: IArray1[Int], n: Int, z: List[Int]) =>
    val f = (a: List[Int], b: Int) => (n + b) :: a
    xs.scanLeft(z)(f).toList must_=== xs.toList.scanLeft(z)(f)
  }

  property("scanRight") = forAll { (xs: IArray1[Int], n: Int, z: List[Int]) =>
    val f = (a: Int, b: List[Int]) => (n + a) :: b
    xs.scanRight(z)(f).toList must_=== xs.toList.scanRight(z)(f)
    xs.scanRight(z)(f) must_=== xs.reverse.scanLeft(z)((a, b) => f(b, a)).reverse
  }
}
