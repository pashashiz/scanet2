package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scanet.test.CustomMatchers

class TensorTest extends AnyFlatSpec with CustomMatchers {

  "scalar tensor" should "be constructed" in {
    Tensor.scalar(5) should (haveShape (Shape()) and containData (Array(5)))
  }

  "vector tensor" should "be constructed" in {
    Tensor.vector(1, 2, 3) should (haveShape (Shape(3)) and containData (Array(1, 2, 3)))
  }

  "matrix tensor" should "be constructed" in {
      println(Tensor.matrix(Array(1, 2, 3), Array(4, 5, 6)).show())
  }

  "n-dim tensor" should "be constructed" in {
    Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8), Shape(2, 2, 2)) should
      (haveShape (Shape(2, 2, 2)) and containData (Array(1, 2, 3, 4, 5, 6, 7, 8)))
  }

  it should "fail to be constructed when shape does not match the input" in {
    an [IllegalArgumentException] should be thrownBy
      Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8), Shape(2, 2, 1))
  }
}
