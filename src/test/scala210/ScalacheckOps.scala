package iarray

import org.scalacheck._

trait ScalacheckOps {
  implicit def toPropOps(self: Prop) = new PropOps(self)
}

final class PropOps(val self: Prop) extends AnyVal{

  // https://github.com/rickynils/scalacheck/pull/85
  def contramap(f: Gen.Params => Gen.Params): Prop =
    new Prop{
      def apply(prms: Prop.Params) =
        self(prms.copy(genPrms = f(prms.genPrms)))
    }
}

