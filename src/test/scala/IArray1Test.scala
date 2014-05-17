package iarray

import scalaz._
import org.scalacheck.Prop.forAll
import std.anyVal._, std.list._, std.option._, std.string._, std.tuple._
import scalaz.scalacheck.ScalazArbitrary._
import IArray.conform

object IArray1Test extends TestCommon {

  property("partition") = forAll { a: IArray1[Int] =>
    val f = (_: Int) % 3 == 0
    a.partition(f) must_=== a.toIArray.partition(f)
  }

  property("dropL") = forAll { (a: IArray1[Int], n: Int) =>
    a.dropL(n).toList must_=== a.toList.drop(n)
  }

  property("toIArray") = forAll { a: IArray1[Int] =>
    a.toIArray must_=== a.to[IArray]
  }

  property("collect") = forAll { a: IArray1[Int] =>
    val f: PartialFunction[Int, String] = { case i if i % 5 == 2 => i.toString }
    a.collect(f).toList must_=== a.toList.collect(f)
  }

  property("unite1") = forAll { a: IArray1[NonEmptyList[Int]] =>
    a.unite1.toNel must_=== Bind[NonEmptyList].join(a.toNel)
  }

  property("unite") = forAll { a: IArray1[List[Int]] =>
    a.unite must_=== a.toList.flatten.to[IArray]
  }

  property("fromOneAnd") = forAll { a: OneAnd[List, Int] =>
    IArray1.fromOneAnd(a).toOneAnd[List] must_=== a
  }

  property("maxOf") = forAll { a: IArray1[Int] =>
    a.maxOf(- _) must_=== a.toList.map(- _).max
  }

  property("minOf") = forAll { a: IArray1[Int] =>
    a.minOf(- _) must_=== a.toList.map(- _).min
  }

  property("maxBy") = forAll { a: IArray1[Int] =>
    a.maxBy(conform) must_=== a.toList.maxBy(conform)
    a.maxBy(- _) must_=== a.toList.maxBy(- _)
  }

  property("minBy") = forAll { a: IArray1[Int] =>
    a.minBy(conform) must_=== a.toList.minBy(conform)
    a.minBy(- _) must_=== a.toList.minBy(- _)
  }

  property("foldl") = forAll { a: IArray1[Int] =>
    a.foldl(List[Int]())((a, b) => b :: a) must_=== a.toList.foldLeft(List[Int]())((a, b) => b :: a)
  }

  property("foldl1") = forAll { a: IArray1[List[Int]] =>
    a.foldl1(_ ::: _) must_=== a.toList.reduceLeft(_ ::: _)
  }

  property("zipAll") = forAll { (as: IArray1[Int], bs: IArray1[Alpha], a: Int, b: Alpha) =>
    as.zipAll(bs, a, b).toList must_=== as.toList.zipAll(bs.toList, a, b)
  }

  property("zipWithIndex") = forAll { a: IArray1[Int] =>
    a.zipWithIndex.toList must_=== a.toList.zipWithIndex
  }

  property("collectFirst collectLast") = forAll{ a: IArray1[Int] =>
    val f: PartialFunction[Int, String] = {case x if x % 10 == 0 => (x * 3).toString }
    a.collectFirst(f) must_=== a.toList.collectFirst(f)
    a.collectLast(f) must_=== a.toList.reverse.collectFirst(f)
  }

  property("cojoin cobind") = forAll{ a: IArray1[Int] =>
    a.cojoin must_=== a.cobind(conform)
    a.cojoin.map(_.toNel).toNel must_=== Comonad[NonEmptyList].cojoin(a.toNel)
  }

  property("sorted") = forAll{ (a: IArray1[Int], b: IArray[Alpha]) =>
    a.sorted.toList must_=== a.toList.sorted
    b.sorted.toList must_=== b.toList.sorted
  }

  property("sortBy") = forAll{ a: IArray1[Int] =>
    a.sortBy(- _).toList must_=== a.toList.sortBy(- _)
  }

  property("sortWith") = forAll{ a: IArray1[Int] =>
    a.sortWith(_ > _).toList must_=== a.toList.sortWith(_ > _)
  }

  property("max min") = forAll{ a: IArray1[Int] =>
    import syntax.foldable1._
    a.max must_=== a.toNel.maximum1
    a.min must_=== a.toNel.minimum1
  }

  property("toIterator") = forAll{ a: Array[Int] =>
    a.toIterator.toList must_=== a.toList
  }

  property("toArray Int") = forAll{ (a: Int, as: Array[Int]) =>
    IArray1(a, as.to[IArray]).toArray.toList must_=== (a +: as).toList
  }

  property("toArray String") = forAll{ (a: Alpha, as: Array[Alpha]) =>
    IArray1(a, as.to[IArray]).toArray.toList must_=== (a +: as).toList
  }

  property("+: :+") = forAll{ (a: Int, as: IArray1[Int]) =>
    (a +: as).toList must_=== a +: as.toList
    (as :+ a).toList must_=== as.toList :+ a
  }

  property("to") = forAll{ as: IArray1[Int] =>
    import syntax.id._
    as.to[List] must_=== as.toList
    (as.toOneAnd[List] |> { x => NonEmptyList.nel(x.head, x.tail) }) must_=== as.toNel
  }

  property("init") = forAll{ as: IArray1[Int] =>
    as.init must_=== as.to[IArray].initOption.get
  }

  property("forall") = forAll{ (as: IArray1[Int], a: Int) =>
    as.forall(_ > a) must_=== as.toList.forall(_ > a)
  }

  property("exists") = forAll{ (as: IArray1[Int], a: Int) =>
    as.exists(_ > a) must_=== as.toList.exists(_ > a)
  }

  property("find") = forAll{ as: IArray1[Int] =>
    as.find(_ % 10 == 0) must_=== as.toList.find(_ % 10 == 0)
  }

  property("findRight") = forAll{ as: IArray1[Int] =>
    as.findRight(_ % 10 == 0) must_=== as.reverse.find(_ % 10 == 0)
  }

  property("contains") = forAll{ (as: IArray1[Int], a: Int) =>
    as.contains(a) must_=== as.toList.contains(a)
  }

  property("reversed") = forAll{ as: IArray1[Int] =>
    as.reversed[List] must_=== as.reverse.toList
  }

  property("reverse") = forAll{ as: IArray1[Int] =>
    as.reverse.toNel must_=== as.toNel.reverse
  }

  property("reverseMap") = forAll{ (as: IArray1[Int], n: Int) =>
    val f = (_: Int) - n
    as.reverseMap(f).toList must_=== as.toList.reverseMap(f)
  }

  property("flatten") = forAll{ as: IArray1[IArray1[Int]] =>
    as.flatten.toList must_=== as.toList.flatMap(_.toList)
  }

  property("indexOfL indexOfR") = forAll{ (as: IArray1[Int], a: Int) =>
    def toOpt(n: Int) = if(n < 0) None else Some(n)
    as.indexOfL(a) must_=== toOpt(as.toList.indexOf(a))
    as.indexOfR(a) must_=== toOpt(as.toList.lastIndexOf(a))
  }

  property("intersperse") = forAll{ xs: IArray1[Alpha] =>
    import syntax.std.list._
    xs.intersperse(Tag(",")).toList must_=== xs.toList.intersperse(Tag(","))
  }

  property("intercalate1") = forAll { xs: IArray1[Alpha] =>
    val xs0: IArray1[String] = Tag.unsubst(xs)
    xs0.intercalate1(",") must_=== Foldable[NonEmptyList].intercalate(xs0.toNel, ",")
  }

  property("equals hashCode law") = forAll { (a1: IArray1[Int], a2: IArray1[Int], a3: IArray1[Int]) =>
    (a1 == a2) must_=== (a2 == a1)
    (a1 == a1.map(conform)) must_=== true
    std.boolean.conditional((a1 == a2) && (a2 == a3), a1 == a3) must_=== true
    std.boolean.conditional(a1 == a2, a1.## == a2.##) must_=== true
  }

  property("fromNel toNel") = forAll { xs: NonEmptyList[Int] =>
    IArray1.fromNel(xs).toNel must_== xs
  }

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

