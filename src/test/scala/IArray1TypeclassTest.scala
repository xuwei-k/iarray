package iarray

import scalaz._, std.anyVal._
import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{Gen, Arbitrary}

object IArray1TypeclassTest extends TestCommon {

  implicit val iarray1Arb0: Arbitrary[IArray1[Int] => Int] = {
    def arb(f: (IArray1[Int], Int) => Int): Gen[IArray1[Int] => Int] =
      implicitly[Arbitrary[Int]].arbitrary.map(a => xs => f(xs, a))

    Arbitrary(Gen.oneOf(
      arb((xs, x) => x),
      arb((xs, _) => xs.head),
      arb((xs, _) => xs.last),
      arb((xs, _) => xs.sum),
      arb((xs, _) => xs(xs.length / 2)),
      arb((xs, x) => xs.head + x),
      arb((xs, x) => xs.last + x),
      arb((xs, _) => xs.size)
    ))
  }

  property("cobind associative") =
    cobind.cobindAssociative[IArray1, Int, Int, Int, Int](
      implicitly, implicitly, implicitly, iarray1Arb0, iarray1Arb0, iarray1Arb0
    )
  checkAll(comonad.laws[IArray1](implicitly, implicitly, iarray1Arb0, implicitly))
  checkAll(plus.laws[IArray1])
  checkAll(monad.laws[IArray1])
  checkAll(align.laws[IArray1])
  checkAll(zip.laws[IArray1])
  checkAll(traverse1.laws[IArray1])
  checkAll(ScalazProperties.apply.laws[IArray1](IArray1ZipApply, implicitly, implicitly, implicitly))

}

