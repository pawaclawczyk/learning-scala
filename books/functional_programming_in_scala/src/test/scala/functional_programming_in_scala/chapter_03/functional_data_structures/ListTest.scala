package functional_programming_in_scala.chapter_03.functional_data_structures

import org.scalatest._

class ListTest extends FunSuite with Matchers {
  test("sum of empty list is 0") {
    List.sum(Nil) shouldBe 0
  }

  test("sum of list elements") {
    List.sum(List(1, 2, 3, 4, 5)) shouldBe 15
  }

  test("product of empty list is 1.0") {
    List.prod(List()) shouldBe 1.0
  }

  test("prodcut of list elements") {
    List.prod(List(1, 2, 3, 4, 5)) shouldBe 120
  }
}
