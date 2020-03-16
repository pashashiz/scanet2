package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scanet.test.CustomMatchers

class TensorTest extends AnyFlatSpec with CustomMatchers {

  "scalar tensor" should "be constructed" in {
    Tensor.scalar(5) should (haveShape (Nil) and containData (Array(5)))
  }

  "vector tensor" should "be constructed" in {
    Tensor.vector(1, 2, 3) should (haveShape (List(3)) and containData (Array(1, 2, 3)))
  }

  "matrix tensor" should "be constructed" in {
    // todo
  }

  "n-dim tensor" should "be constructed" in {
    Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8), List(2L, 2L, 2L)) should
      (haveShape (List(2L, 2L, 2L)) and containData (Array(1, 2, 3, 4, 5, 6, 7, 8)))
  }

  // todo: incorrect tensors
}
