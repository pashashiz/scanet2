package org.scanet

import org.scanet.core.{ConvertableFrom, ConvertableInstances, ConvertableTo, Dist, DistInstances, Eq, Field, Numeric, NumericInstances, Order, Rig, Ring, Rng, Semiring}
import org.scanet.linalg.{IndexConversions, Slice}

package object syntax {

  trait CoreSyntax extends ConvertableInstances with NumericInstances with DistInstances
    with Semiring.ToSemiringOps with Rng.ToRngOps with Rig.ToRigOps with Ring.ToRingOps with Field.ToFieldOps
    with Eq.ToEqOps with Order.ToOrderOps
    with ConvertableTo.ToConvertableToOps with ConvertableFrom.ToConvertableFromOps
    with Numeric.ToNumericOps
    with Dist.ToDistOps

  object core extends CoreSyntax {}

  trait LinalgSyntax extends CoreSyntax with IndexConversions

  object linalg extends LinalgSyntax with Slice.Syntax
}
