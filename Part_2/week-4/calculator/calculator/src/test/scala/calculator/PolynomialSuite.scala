package calculator

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSuite, ShouldMatchers}

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {

  test("compute delta with constant signal") {
    val result = Polynomial.computeDelta(Signal(1), Signal(1), Signal(1))
    assert(result() == -3)

    val a = 1.0
    val b = 100.0
    val c = 3.0
    val result2 = Polynomial.computeDelta(Signal(a), Signal(b), Signal(c))    
    assert(result2() == (b * b) - 4 * a * c)
  }

  test("compute solutions with constant signal") {
    val a = 2.0
    val b = -5.0
    val c = -3.0
    val delta = 49.0
    val result2 = Polynomial.computeSolutions(Signal(a), Signal(b), Signal(c), Signal(delta))

    assert(result2() == Set(3.0, -0.5))
  }

  test("compute solutions with negative delta") {
    val a = 1.0
    val b = 3.0
    val c = 3.0
    val delta = -2.0
    val result2 = Polynomial.computeSolutions(Signal(a), Signal(b), Signal(c), Signal(delta))

    assert(result2() == Set.empty)
  }
}
