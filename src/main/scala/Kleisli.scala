package iarray

import scalaz.{OneAnd, NonEmptyList, Kleisli, Endomorphic}
import scalaz.std.option._

private object kleisli {

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

  val tailOptionEndo = Endomorphic.endoKleisli(tailOption.run)

  val initOptionEndo = Endomorphic.endoKleisli(initOption.run)

}

