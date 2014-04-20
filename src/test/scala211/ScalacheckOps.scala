package iarray

import org.scalacheck._

trait ScalacheckOps {
  implicit def toPropOps(self: Prop) = new PropOps(self)
}

final class PropOps(val self: Prop) extends AnyVal{

  // https://github.com/rickynils/scalacheck/pull/85
  def contramap(f: Gen.Parameters => Gen.Parameters): Prop =
    new Prop{
      def apply(prms: Gen.Parameters) = self(f(prms))
    }
}

