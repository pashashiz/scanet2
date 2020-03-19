package org.scanet.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scanet.test.CustomMatchers

class NativeArrayTest extends AnyFlatSpec with CustomMatchers  {

  "native array" should "show the content" in {
    NativeArray(1, 2, 3).show(2) should be("[1, 2...]")
  }

  "two same arrays" should "be equal" in {
    NativeArray(1, 2, 3) should be(NativeArray(1, 2, 3))
  }

  it should "have same hash codes" in {
    NativeArray(1, 2, 3).hashCode() should be(NativeArray(1, 2, 3).hashCode())
  }

  "two different arrays" should "not be equal" in {
    NativeArray(1, 2, 3) should not be NativeArray(1, 2, 4)
  }

  "pointer" should "not have same hash codes" in {
    NativeArray(1, 2, 3).hashCode() should not be NativeArray(1, 2, 4).hashCode()
  }

  it should "show right capacity" in {
    NativeArray(1, 2, 3).capacity should be(3)
  }

  it should "show right default position" in {
    NativeArray(1, 2, 3).position should be(0)
  }

  it should "show allow changing a position" in {
    val arr = NativeArray(1, 2, 3)
    arr.position(1)
    arr.position should be(1)
    arr should be(NativeArray(2, 3))
  }

  it should "crash when setting position out of bound" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 2, 3).position(5)
  }

  it should "show right default limit" in {
    NativeArray(1, 2, 3).limit should be(3)
  }

  it should "show allow changing a limit" in {
    val arr = NativeArray(1, 2, 3)
    arr.limit(2)
    arr.limit() should be(2)
    arr should be(NativeArray(1, 2))
  }

  it should "crash when setting limit out of bound" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 2, 3).limit(10)
  }

  it should "allow getting element by index" in {
    NativeArray(1, 5, 10).get(2) should be(10)
  }

  it should "allow include position offset when getting element by index" in {
    NativeArray(1, 5, 10).position(1).get(0) should be(5)
  }

  it should "crash when getting element by index which is out of bound" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 5, 10).get(10)
  }

  it should "allow putting element by index" in {
     NativeArray(1, 5, 10).put(1, 7).put(2, 15) should be(NativeArray(1, 7, 15))
  }

  it should "include position offset when putting element by index" in {
     NativeArray(1, 5, 10).position(1).put(1, 7) should be(NativeArray(5, 7))
  }

  it should "crash when putting element by index which is out of bound" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 5, 10).put(10, 10)
  }

  it should "allow putting an array" in {
    NativeArray(1, 5, 10).putAll(Array(2, 6)) should be(NativeArray(2, 6, 10))
  }

  it should "crash when putting an array longer then an original" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 5, 10).putAll(Array(2, 6, 7, 8))
  }

  it should "crash when putting an array longer then an original including position" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 5, 10).position(1).putAll(Array(2, 6, 7))
  }

  it should "crash when putting an array longer then an original including limit" in {
    an [IllegalArgumentException] should be thrownBy NativeArray(1, 5, 10).limit(2).putAll(Array(2, 6, 7))
  }

  "pointer of one type" should "be converted into pointer of another type" in {
    NativeArray(1L, 2L, 3L).to[Int] should be(NativeArray(1, 0, 2, 0, 3, 0))
  }

  "pointer" should "be converted into buffer" in {
    NativeArray(1, 2, 3).asBuffer should
      be("Buffer[Int](capacity=3, position=0, limit=3, direct=true)[1, 2, 3]")
  }
}
