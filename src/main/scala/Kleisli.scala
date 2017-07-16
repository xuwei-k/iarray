package iarray

import scalaz._
import scalaz.std.option._

private object kleisli {

  val toList: Kleisli[List, IArray[AnyRef], AnyRef] =
    Kleisli(_.toList)

  val zipWithIndex: Kleisli[IArray, IArray[AnyRef], (AnyRef, Int)] =
    Kleisli(_.zipWithIndex)

  val reverse: Kleisli[IArray, IArray[AnyRef], AnyRef] =
    Kleisli(_.reverse)

  val toIList: Kleisli[IList, IArray[AnyRef], AnyRef] =
    Kleisli(_.toIList)

  val reverseList: Kleisli[List, IArray[AnyRef], AnyRef] =
    Kleisli(_.reverseList)

  val reverseIList: Kleisli[IList, IArray[AnyRef], AnyRef] =
    Kleisli(_.reverseIList)

  val zipperEnd: Kleisli[Option, IArray[AnyRef], Zipper[AnyRef]] =
    Kleisli(_.zipperEnd)

  val cojoin: Kleisli[IArray, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.cojoin)

  val toNel: Kleisli[Option, IArray[AnyRef], NonEmptyList[AnyRef]] =
    Kleisli(_.toNel)

  val toIArray1: Kleisli[Option, IArray[AnyRef], IArray1[AnyRef]] =
    Kleisli(_.toIArray1)

  val oneAnd: Kleisli[Option, IArray[AnyRef], OneAnd[IArray, AnyRef]] =
    Kleisli(_.oneAnd)

  val headOption: Kleisli[Option, IArray[AnyRef], AnyRef] =
    Kleisli(_.headOption)

  val headMaybe: Kleisli[Maybe, IArray[AnyRef], AnyRef] =
    Kleisli(_.headMaybe)

  val lastOption: Kleisli[Option, IArray[AnyRef], AnyRef] =
    Kleisli(_.lastOption)

  val lastMaybe: Kleisli[Maybe, IArray[AnyRef], AnyRef] =
    Kleisli(_.lastMaybe)

  val tailOption: Kleisli[Option, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.tailOption)

  val tailMaybe: Kleisli[Maybe, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.tailMaybe)

  val initOption: Kleisli[Option, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.initOption)

  val initMaybe: Kleisli[Maybe, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.initMaybe)

  val tailOptionEndo = Endomorphic.endoKleisli(tailOption.run)

  val tailMaybeEndo = Endomorphic.endoKleisli(tailMaybe.run)

  val initOptionEndo = Endomorphic.endoKleisli(initOption.run)

  val initMaybeEndo = Endomorphic.endoKleisli(initMaybe.run)

}
