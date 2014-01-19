package iarray

import scalaz._
import Isomorphism._
import org.scalacheck.{Gen, Arbitrary}
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._

trait TestCommon extends SpecLite {
 
  sealed trait AlphaTag
  type Alpha = String @@ AlphaTag

  implicit val alpha: Arbitrary[Alpha] = Tag.subst(Arbitrary(Gen.alphaStr))
  implicit val alphaShow: Show[Alpha] = Show.showA
  implicit val alphaEqual: Equal[Alpha] = Equal.equalA

  implicit def arb[A: Arbitrary]: Arbitrary[IArray[A]] =
    Functor[Arbitrary].map(implicitly[Arbitrary[List[A]]])(IArray.fromList[A])

  implicit def iarrayShow[A: Show]: Show[IArray[A]] =
    Show.showA

  val tryEitherIso: ({type λ[α] = Throwable \/ α})#λ <~> scala.util.Try =
    new IsoFunctorTemplate[({type λ[α] = Throwable \/ α})#λ, scala.util.Try] {
      def from[A](ga: scala.util.Try[A]) = ga match {
        case scala.util.Success(a) => \/-(a)
        case scala.util.Failure(e) => -\/(e)
      }
      def to[A](fa: Throwable \/ A) = fa match {
        case \/-(a) => scala.util.Success(a)
        case -\/(e) => scala.util.Failure(e)
      }
    }

  implicit def tryArb[A: Arbitrary]: Arbitrary[scala.util.Try[A]] =
    Functor[Arbitrary].map(
      implicitly[Arbitrary[Throwable \/ A]]
    )(tryEitherIso.to(_))
}

