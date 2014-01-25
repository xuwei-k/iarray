package iarray

import scalaz._
import std.anyVal._, std.string._
import scalaz.scalacheck.ScalazProperties._

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

  checkAll(monadPlus.strongLaws[IArray])
  checkAll(isEmpty.laws[IArray])
  checkAll(foldable.laws[IArray])
  checkAll(zip.laws[IArray])
  checkAll(align.laws[IArray])
  checkAll(cobind.laws[IArray])
  checkAll(monoid.laws[IArray[String]])

}

