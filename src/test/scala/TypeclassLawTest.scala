package iarray

import scalaz._
import std.anyVal._, std.string._
import org.scalacheck._
import scalaz.scalacheck.ScalazProperties._

object IArrayTraverseTest extends TestCommon {

  for ((name, prop) <- traverse.laws[IArray].properties) yield {
    property(name) = prop.check(new Test.Parameters.Default{
      override val maxSize = 5
    })
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

