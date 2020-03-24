package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scanet.test.CustomMatchers
import org.scanet.instances.core._

class TensorTest extends AnyFlatSpec with CustomMatchers {

  "scalar tensor" should "be allocated" in {
    Tensor.scalar(5) should
      (haveShape (Shape()) and containData (Array(5)))
  }

  "vector tensor" should "be allocated" in {
    Tensor.vector(1, 2, 3) should
      (haveShape (Shape(3)) and containData (Array(1, 2, 3)))
  }

  "matrix tensor" should "be allocated" in {
      Tensor.matrix(Array(1, 2, 3), Array(4, 5, 6)) should
        (haveShape (Shape(2, 3)) and containData (Array(1, 2, 3, 4, 5, 6)))
  }

  "n-dim tensor" should "be allocated" in {
    Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8), Shape(2, 2, 2)) should
      (haveShape (Shape(2, 2, 2)) and containData (Array(1, 2, 3, 4, 5, 6, 7, 8)))
  }

  it should "fail to be constructed when shape does not match the input" in {
    an [IllegalArgumentException] should be thrownBy
      Tensor(Array(1, 2, 3, 4, 5, 6, 7, 8), Shape(2, 2, 1))
  }

  "zeros tensor" should "be allocated" in {
    Tensor.zeros[Int](2, 2) should be(Tensor.matrix(Array(0, 0), Array(0, 0)))
  }

  "tensor" should "be filled with a given number" in {
    Tensor.fill(2, 2)(1) should be(Tensor.matrix(Array(1, 1), Array(1, 1)))
  }

  it should "be tabulated" in {
     Tensor.tabulate[Int](2, 2)((i, j) => (i + 1) * (j + 1)) should
       be(Tensor.matrix(Array(1, 2), Array(2, 4)))
    // Tensor.zeros[Int](2,2,2)
  }

}
