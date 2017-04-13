package BookClub.Datastructures

import org.scalatest._
import BookClub.Datastructures.List._

class ListSpec extends FlatSpec with Matchers {
  "An empty list" should "have sum equal to 0" in {
    sum(Nil) should be (0)
  }

  it should "have product equal 1.0" in {
    product(Nil) should be (1.0)
  }

  it should "have tail Nil" in {
    tail(Nil) should be (Nil)
  }

  it should "be List(head) after setHead(4)" in {
    setHead(4, Nil) should be (Cons(4, Nil))
  }

  it should "be Nil after dropping 2 elements" in {
    drop(Nil, 2) should be (Nil)
  }

  it should "have length of 0" in {
    List.length(Nil) should be (0)
  }

  "A List(1, 2, 3)" should "have sum equal to 6" in {
    sum(List(1, 2, 3)) should be (6)
  }

  it should "have product equal to 6" in {
    product(List(1, 2, 3)) should be (6)
  }

  it should "have tail List(2, 3)" in {
    tail(List(1, 2, 3)) should be (List(2, 3))
  }

  it should "be List(4, 2, 3) after setHead(4)" in {
    setHead(4, List(1, 2, 3)) should be (List(4, 2, 3))
  }

  it should "be List(3) after dropping 2 elements" in {
    drop(List(1, 2, 3), 2) should be (List(3))
  }

  it should "be empty after dropping 3 elements" in {
    drop(List(1, 2, 3), 3) should be (Nil)
  }

  it should "be the same after dropping 0 elements" in {
    drop(List(1, 2, 3), 0) should be (List(1, 2, 3))
  }

  it should "have legth of 3" in {
    List.length(List(1, 2, 3)) should be (3)
  }

  "length with left fold" should "equal 4" in {
    leftLength(List(4, 2, 6, 8)) should be (4)
  }

  "reverse of List(7, 4, 3, 2)" should "be equal to List(2, 3, 4, 7)" in {
    reverse(List(7, 4, 3, 2)) should be (List(2, 3, 4, 7))
  }

  "leftFold in terms of rightFold" should "works fine" in {
    leftAsRight(List(1, 2, 3, 4, 5), 15)(_ - _) should be (0)
  }

  "rightFold in terms of leftFold" should "works fine" in {
    rightAsLeft(List(1, 2, 3, 4, 5), 15)(_ - _) should be (-12)
  }
}
