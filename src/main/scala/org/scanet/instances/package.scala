package org.scanet

import org.scanet.core.{ConvertableFrom, ConvertableInstances, ConvertableTo, Field, Numeric, NumericInstances, Rig, Ring, Rng, Semiring}

package object instances {
  object core extends ConvertableInstances with NumericInstances with Semiring.ToSemiringOps with Rng.ToRngOps with Rig.ToRigOps
    with Ring.ToRingOps with Field.ToFieldOps with Numeric.ToNumericOps
    with ConvertableTo.ToConvertableToOps with ConvertableFrom.ToConvertableFromOps {}
}
