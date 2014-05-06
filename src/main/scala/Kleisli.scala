package iarray

import scalaz._

private object kleisli {

  val toList: Kleisli[List, IArray[AnyRef], AnyRef] =
    Kleisli(_.toList)

  val reverseList: Kleisli[List, IArray[AnyRef], AnyRef] =
    Kleisli(_.reverseList)

  val zipperEnd: Kleisli[Option, IArray[AnyRef], Zipper[AnyRef]] =
    Kleisli(_.zipperEnd)

  val toNel: Kleisli[Option, IArray[AnyRef], NonEmptyList[AnyRef]] =
    Kleisli(_.toNel)

  val toIArray1: Kleisli[Option, IArray[AnyRef], IArray1[AnyRef]] =
    Kleisli(_.toIArray1)

  val oneAnd: Kleisli[Option, IArray[AnyRef], OneAnd[IArray, AnyRef]] =
    Kleisli(_.oneAnd)

  val headOption: Kleisli[Option, IArray[AnyRef], AnyRef] =
    Kleisli(_.headOption)

  val lastOption: Kleisli[Option, IArray[AnyRef], AnyRef] =
    Kleisli(_.lastOption)

  val tailOption: Kleisli[Option, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.tailOption)

  val initOption: Kleisli[Option, IArray[AnyRef], IArray[AnyRef]] =
    Kleisli(_.initOption)

}

