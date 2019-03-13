package iarray

import scala.reflect.ClassTag
import scalaz._
import scalaprops._
import scalaprops.ScalapropsScalaz._
import Isomorphism._
import std.list._, std.anyVal._

trait TestCommon extends Scalaprops {
  trait ShowAndEq[A] {
    implicit val e: Equal[A] = Equal.equalA[A]
    implicit val s: Show[A] = Show.showA[A]
  }

  val tryEitherIso: ({ type λ[α] = Throwable \/ α })#λ <~> scala.util.Try =
    new IsoFunctorTemplate[({ type λ[α] = Throwable \/ α })#λ, scala.util.Try] {
      def from[A](ga: scala.util.Try[A]) = ga match {
        case scala.util.Success(a) => \/-(a)
        case scala.util.Failure(e) => -\/(e)
      }
      def to[A](fa: Throwable \/ A) = fa match {
        case \/-(a) => scala.util.Success(a)
        case -\/(e) => scala.util.Failure(e)
      }
    }

  final implicit def iarrayGen[A: Gen]: Gen[IArray[A]] =
    Gen[List[A]].map(IArray.fromList)

  final implicit def iarrayCogen[A: Cogen]: Cogen[IArray[A]] =
    Cogen[List[A]].contramap(_.toList)

  final implicit def iarray1Gen[A: Gen]: Gen[IArray1[A]] =
    Gen[NonEmptyList[A]].map(IArray1.fromNel)

  final implicit def iarray1Cogen[A: Cogen]: Cogen[IArray1[A]] =
    Cogen[NonEmptyList[A]].contramap(_.toNel)

  final implicit def iarrayShrink[A: Shrink]: Shrink[IArray[A]] =
    Shrink[List[A]].xmap(IArray.fromList, _.toList)

  final implicit val genString: Gen[String] =
    scalaprops.Gen.asciiString.mapSize(_ / 4)

  implicit class AnyOps[A](actual: => A) {
    def must_===(expected: A)(implicit S: Show[A], A: Equal[A]): Boolean = {
      val act = actual
      def test = A.equal(expected, act)
      if (!test) {
        sys.error(s"${S.show(act)} !== ${S.shows(expected)}")
      }
      test
    }

    def mustThrowA[T <: Throwable](implicit man: ClassTag[T]): Boolean = {
      val erasedClass = man.runtimeClass
      try {
        actual
        sys.error("no exception thrown, expected " + erasedClass)
      } catch {
        case ex: Throwable =>
          if (!erasedClass.isInstance(ex)) {
            sys.error("wrong exception thrown, expected: " + erasedClass + " got: " + ex)
          } else {
            true
          }
      }
    }

  }

  final implicit val throwableGen: Gen[Throwable] =
    Gen.oneOf(
      Gen.value(new Throwable),
      Gen[String].map(new Throwable(_))
    )

  final implicit def tryGen[A: Gen]: Gen[scala.util.Try[A]] =
    Gen[Throwable \/ A].map(tryEitherIso.to(_))
}
