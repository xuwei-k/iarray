package iarray

import scalaprops._
import scalaz.std.anyVal._

object IArray1TypeclassTest extends TestCommon {

  val laws = Properties.list(
    scalazlaws.monad.all[IArray1],
    scalazlaws.plus.all[IArray1],
    scalazlaws.zip.all[IArray1],
    scalazlaws.align.all[IArray1],
    scalazlaws.traverse1.all[IArray1],
    scalazlaws.comonad.all[IArray1]
  )

}
