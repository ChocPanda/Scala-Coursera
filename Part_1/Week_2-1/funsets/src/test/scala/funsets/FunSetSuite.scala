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
    assert(contains(_ => true, 100))
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
      assert(contains(s1, 1), "SingletonSet s1 does not contain 1")
      assert(contains(s2, 2), "SingletonSet s2 does not contain 2")
      assert(contains(s3, 3), "SingletonSet s3 does not contain 3")
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

  test("intersect contains all the elements in both sets") {
    new TestSets {
      val ts1 = union(s1, s2)
      val ts2 = union(s1, s3)

      val intersection = intersect(ts1, ts2)

      assert(contains(intersection, 1))
      assert(!contains(intersection, 2))
      assert(!contains(intersection, 3))
    }
  }

  test("intersect contains all the elements in only in the first set") {
    new TestSets {
      val ts1 = union(s1, s2)
      val ts2 = union(s1, s3)

      val difference = diff(ts1, ts2)

      assert(contains(difference, 2))
      assert(!contains(difference, 3))
      assert(!contains(difference, 1))
    }
  }

  test("filter contains only elements in the set and that satisfy the predicate") {
    new TestSets {
      val f1 = filter(s1, _ => false)
      val f2 = filter(s1, _ => true)

      assert(!contains(f1, 1))
      assert(!contains(f1, 2))

      assert(contains(f2, 1))
      assert(!contains(f2, 2))
    }
  }
  
  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      assert(forall(s1, _ => true))
      assert(!forall(s1, _ => false))
      
      val ts1 = union(s1, s2)
      
      assert(!forall(ts1, n => contains(s2, n)))
    }
  }
  
  test("exists returns whether there exists a bounded integer within `s` that satisfies `p`") {
    new TestSets {
      assert(exists(s1, _ => true))
      assert(!exists(s1, _ => false))

      val ts1 = union(s1, s2)

      assert(exists(ts1, n => contains(s2, n)))
    }
  }

  test("map returns a set transformed by applying `f` to each element of `s`") {
    new TestSets {
      val ts1 = map(s1, n => n + 5)
      assert(contains(ts1, 6))
      assert(!contains(ts1, 1))
    }
  }

}
