package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val empty = (x: Int) => false
    val integers = (x: Int) => true
    def divisibleBy(n: Int): Set = (x: Int) => 0 == x % n
    def greaterThan(n: Int): Set = (x: Int) => x > n
    def lessThan(n: Int): Set = (x: Int) => x < n
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains elements that exist in both sets") {
    new TestSets {
      val s = intersect(divisibleBy(2), divisibleBy(3))

      assert(contains(s, 6), "Should contain 6")
      assert(!contains(s, 4), "Should not contain 4")
      assert(!contains(s, 9), "Should not contain 9")
    }
  }

  test("difference contains elements from first set that does not exist in second one") {
    new TestSets {
      val s = diff(divisibleBy(2), divisibleBy(4))

      assert(contains(s, 2), "Should contain 2")
      assert(!contains(s, 4), "Should not contain 4")
    }
  }

  test("filter returns the subset of S that holds p") {
    new TestSets {
      val s = filter(divisibleBy(2), divisibleBy(3))

      assert(contains(s, 2 * 3), "Should contain 6")
      assert(!contains(s, 2 * 2), "Should not contain 4")
    }
  }

  test("all elements in S satisfies p") {
    new TestSets {
      assert(forall(divisibleBy(4), divisibleBy(2)), "all integers divisible by 4 are divisible by 2")
      assert(!forall(divisibleBy(2), divisibleBy(4)), "not all integers divisible by 2 are divisible by 4")
    }
  }

  test("exists element in S that satisfies p") {
    new TestSets {
      assert(exists(divisibleBy(2), divisibleBy(4)), "in set of integers divisible by 2 exists integers divisible by 4")
      assert(!exists(greaterThan(0), lessThan(0)), "in set of integers greater than 0 does not exist integer that is less than 0")
    }
  }

  test("Play around with empty set and set of integers") {
    new TestSets {
      assert(!contains(empty, 0))
      assert(contains(integers, 0))

      assert(forall(integers, union(integers, empty)))

      assert(!exists(integers, intersect(integers, empty)))
      assert(forall(empty, intersect(integers, empty)))
    }
  }

  test("map elements between sets") {
    new TestSets {
      val s = map(integers, (x: Int) => x * 2)

      assert(contains(s, 2))
      assert(!contains(s, 1))
    }
  }
}
