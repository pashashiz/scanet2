package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scanet.core.Generator.uniform
import org.scanet.test.CustomMatchers
import org.scanet.syntax.core._

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
  }

  "diagonal matrix" should "be created" in {
    Tensor.diag[Int](1, 2, 3) should be(
      Tensor.matrix(
        Array(1, 0, 0),
        Array(0, 2, 0),
        Array(0, 0, 3)))
  }

  "eye matrix" should "be created" in {
    Tensor.eye[Int](3) should be(
      Tensor.matrix(
        Array(1, 0, 0),
        Array(0, 1, 0),
        Array(0, 0, 1)))
  }

  "linspace vector" should "be created for Int" in {
    Tensor.linspace(2, 10, 5) should
      be(Tensor.vector(2, 4, 6, 8, 10))
  }

  it should "be created for Float" in {
    Tensor.linspace(2.0f, 4.0f, 5) should
      be(Tensor.vector(2.0f, 2.5f, 3.0f, 3.5f, 4.0f))
  }

  "vector" should "be created from a range" in {
    Tensor.vector(1 to 10 by 2) should
      be(Tensor.vector(1, 3, 5, 7, 9))
  }

  it should "be created from exclusive range" in {
    Tensor.range(1, 5, 2) should be(Tensor.vector(1, 3))
  }

  it should "be created from exclusive range with float" in {
    Tensor.range(1.0f, 5.0f, 2.1f) should be(Tensor.vector(1f, 3.1f))
  }

  it should "be created from inclusive range" in {
    Tensor.range(1, 5, 2, inclusive = true) should be(Tensor.vector(1, 3, 5))
  }

  it should "be created from inclusive range with float" in {
    Tensor.range(1.0f, 6.0f, 2.5f, inclusive = true) should be(Tensor.vector(1f, 3.5f, 6.0f))
  }

  "random Int tensor" should "be created with uniform distribution" in {
    Tensor.rand[Int](Shape(3), uniform(1)) should
      be(Tensor.vector(384748, -1151252339, -549383847))
  }

  "random Float tensor" should "be created with uniform distribution" in {
    Tensor.rand[Float](Shape(3), uniform(1)) should
      be(Tensor.vector(8.952618E-5f, 0.73195314f, 0.8720866f))
  }

  "random Double tensor" should "be created with uniform distribution" in {
    Tensor.rand[Double](Shape(3), uniform(1L)) should
      be(Tensor.vector(8.958178688844853E-5, 0.872086605065287, 0.7943048233411579))
  }

  "matrix" should "be indexed by row number" in {
    Tensor.eye[Int](3).get(Index(1)) should be(Tensor.vector(0, 1, 0))
  }
}
