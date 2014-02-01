package iarray

import scalaz._
import std.anyVal._, std.string._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{Gen, Arbitrary}

object IArrayTraverseTest extends TestCommon with ScalacheckOps {

  for ((name, prop) <- traverse.laws[IArray].properties) yield {
    property(name) = {
      if(name contains "sequential fusion")
        prop.contramap(p => p.resize(p.size % 6))
      else
        prop
    }
  }

}

object TypeclassLawTest extends TestCommon {

  implicit val iarrayArb0: Arbitrary[IArray[Int] => Int] = {
    def arb(f: (IArray[Int], Int) => Int): Gen[IArray[Int] => Int] =
      implicitly[Arbitrary[Int]].arbitrary.map(a => xs => f(xs, a))

    Arbitrary(Gen.oneOf(
      arb((_ , x) => x),
      arb((xs, x) => xs.headOption getOrElse x),
      arb((xs, x) => xs.lastOption getOrElse x),
      arb((xs, _) => xs.sum),
      arb((xs, x) => xs.max.getOrElse(x)),
      arb((xs, x) => xs.min.getOrElse(x)),
      arb((xs, x) => if(xs.nonEmpty) xs(xs.length / 2) else x),
      arb((xs, x) => xs.headOption.map(_ + x).getOrElse(- x)),
      arb((xs, x) => xs.lastOption.map(_ + x).getOrElse(- x)),
      arb((xs, _) => xs.size)
    ))
  }

  property("cobind associative") =
    cobind.cobindAssociative[IArray, Int, Int, Int, Int](
      implicitly, implicitly, implicitly, iarrayArb0, iarrayArb0, iarrayArb0
    )

  checkAll(monadPlus.strongLaws[IArray])
  checkAll(isEmpty.laws[IArray])
  checkAll(foldable.laws[IArray])
  checkAll(zip.laws[IArray])
  checkAll(align.laws[IArray])
  checkAll(monoid.laws[IArray[String]])

}

