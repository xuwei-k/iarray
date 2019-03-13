package iarray

import scalaprops._
import scalaprops.ScalapropsScalaz._
import scalaz.std.anyVal._

object ScalapropsTest extends TestCommon {

  val laws = Properties.list(
    scalazlaws.monadPlus.all[IArray],
    scalazlaws.isEmpty.all[IArray],
    scalazlaws.zip.all[IArray],
    scalazlaws.align.all[IArray],
    scalazlaws.traverse.all[IArray],
    scalazlaws.cobind.all[IArray]
  )

}
