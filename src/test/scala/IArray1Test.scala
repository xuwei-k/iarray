package iarray

import scalaz._
import org.scalacheck.Prop.forAll
import std.anyVal._, std.list._, std.option._, std.string._
import scalaz.scalacheck.ScalazArbitrary._

object IArray1Test extends TestCommon {

  property("cojoin cobind") = forAll{ a: IArray1[Int] =>
    a.cojoin must_=== a.cobind(conforms)
    a.cojoin.map(_.toNel).toNel must_=== Comonad[NonEmptyList].cojoin(a.toNel)
  }

  property("max min") = forAll{ a: IArray1[Int] =>
    import syntax.foldable1._
    a.max must_=== a.toNel.maximum1
    a.min must_=== a.toNel.minimum1
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

  property("contains") = forAll{ (as: IArray1[Int], a: Int) =>
    as.contains(a) must_=== as.toList.contains(a)
  }

  property("reverse") = forAll{ as: IArray1[Int] =>
    as.reverse.toNel must_=== as.toNel.reverse
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
    (a1 == a1.map(conforms)) must_=== true
    std.boolean.conditional((a1 == a2) && (a2 == a3), a1 == a3) must_=== true
    std.boolean.conditional(a1 == a2, a1.## == a2.##) must_=== true
  }

  property("fromNel toNel") = forAll { xs: NonEmptyList[Int] =>
    IArray1.fromNel(xs).toNel must_== xs
  }

}

