package org.scanet

import org.scanet.core.{ConvertableFrom, ConvertableInstances, ConvertableTo, Field, Numeric, NumericInstances, Rig, Ring, Rng, Semiring, Eq, Order}

package object instances {
  object core extends ConvertableInstances with NumericInstances
    with Semiring.ToSemiringOps with Rng.ToRngOps with Rig.ToRigOps with Ring.ToRingOps with Field.ToFieldOps
    with Eq.ToEqOps with Order.ToOrderOps
    with ConvertableTo.ToConvertableToOps with ConvertableFrom.ToConvertableFromOps
    with Numeric.ToNumericOps {}
}
