package iarray

import scalaz._, std.anyVal._
import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._

object IArray1Traverse1Test extends TestCommon with ScalacheckOps {

  for ((name, prop) <- traverse1.laws[IArray1].properties) yield {
    property(name) = {
      if(name contains "sequential fusion")
        prop.contramap(p => p.resize(p.size % 6))
      else
        prop
    }
  }

}

object IArray1TypeclassTest extends TestCommon {

  checkAll(comonad.laws[IArray1])
  checkAll(plus.laws[IArray1])
  checkAll(monad.laws[IArray1])
  checkAll(align.laws[IArray1])
  checkAll(zip.laws[IArray1])
  checkAll(foldable1.laws[IArray1])
  checkAll(ScalazProperties.apply.laws[IArray1](IArray1ZipApply, implicitly, implicitly, implicitly))

}

