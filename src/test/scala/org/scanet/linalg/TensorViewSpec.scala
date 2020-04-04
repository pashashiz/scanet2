package org.scanet.linalg

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scanet.linalg.Slice.syntax._

class TensorViewSpec extends AnyFlatSpec with Matchers {

  "view" should "produce right projection when same size" in {
    View(Shape(5, 5, 5), Projection(1, 2 until 4, ::))
      .projectedShapeShort should be(Shape(2, 5))
  }

  it should "produce right projection when smaller size" in {
    View(Shape(5, 5, 5), Projection(1))
      .projectedShapeShort should be(Shape(5, 5))
  }

  it should "produce right projection when index" in {
    View(Shape(5, 5, 5), Projection(1, 1, 1))
      .projectedShapeShort should be(Shape())
  }

  it should "produce right projection when unbound right" in {
    View(Shape(5, 5), Projection(::, 3 until -1))
      .projectedShapeShort should be(Shape(5, 2))
  }

  it should "fail when projection has higher rank" in {
    an [IllegalArgumentException] should be thrownBy
      View(Shape(5, 5), Projection(1, 1, 1))
  }

  it should "fail when projection is out of bound" in {
    an [IllegalArgumentException] should be thrownBy
      View(Shape(5, 5), Projection(6))
  }

  it should "be created from shape with unbound default projection" in {
    View(Shape(5, 5)).projection should be(Projection(0 until 5, 0 until 5))
  }

  "projection" should "be filled from other at left side" in {
    Projection(0 until 5).alignLeft(2, 1.build) should
      be(Projection(1, 0 until 5))
  }

  it should "be filled from other at right side" in {
    Projection(0 until 5).alignRight(2, 1.build) should
      be(Projection(0 until 5, 1))
  }

  it should "should be projected right first time" in {
    val view = View(Shape(5, 5, 5)) narrow Projection(1, 2 until 4, ::)
    view.projectedShapeShort should be(Shape(2, 5))
  }

  "view" should "should be projected right second time" in {
    val view = View(Shape(5, 5, 5)) narrow Projection(1, 2 until 4) narrow Projection(0 until 2)
    view.projectedShapeShort should be(Shape(2, 5))
  }

  it should "should fail to narrow when projection rank is greater" in {
    an [IllegalArgumentException] should be thrownBy
      View(Shape(5, 5)).narrow(Projection(1, 2 until 4, ::))
  }

  it should "should fail to narrow when projection is out of bound" in {
    an [IllegalArgumentException] should be thrownBy
      View(Shape(5, 5)).narrow(Projection(10, 10))
  }

  it should "produce positions when taking :: projection" in {
    val view = View(Shape(3, 3)) narrow Projection(::, ::)
    view.positions.toList should be(List(0, 1, 2, 3, 4, 5, 6, 7, 8))
  }

  it should "produce positions when taking first row" in {
    val view = View(Shape(5, 5)) narrow Projection(0, ::)
    view.positions.toList should be(List(0, 1, 2, 3, 4))
  }

  it should "produce positions when taking second row" in {
    val view = View(Shape(5, 5)) narrow Projection(1, ::)
    view.positions.toList should be(List(5, 6, 7, 8, 9))
  }

  it should "produce position when taking index" in {
    val view = View(Shape(5, 5)) narrow Projection(1, 3)
    view.positions.toList should be(List(8))
  }

  it should "produce positions when taking nested projection" in {
    val view = View(Shape(5, 5, 5)) narrow Projection(1, 2 until 4, ::)
    view.positions.toList should be(List(35, 36, 37, 38, 39, 40, 41, 42, 43, 44))
  }

  "absolute position" should "be found by using index on view" in {
    View(Shape(5, 5)) positionOf Index(1, 4) should be(9)
  }

  it should "produce positions when taking nested projection" in {
    // View(Shape(100, 100, 100)).positions.toList // 13
    val start = System.currentTimeMillis()
//     (0 to 1000000).toList
//    val arr = Array.ofDim[Int](1000000)
//    var i = 0
//    while (i < 1000000) {
//      arr(i) = i
//      i = i + 1x
//    }
    val s = Stream.range(0, 1000000).toList
    val end = System.currentTimeMillis()
    println(s.size)
    println(end - start)
  }

}
