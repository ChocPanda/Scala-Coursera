package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Signal("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Signal(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Signal("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Signal(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Signal(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Signal(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Signal(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Signal(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Signal(-5))
    assert(resultRed2() == "red")
  }

  test("compute values for each expression type alone") {
    val literalResult = Calculator.computeValues(Map(("literal", Signal(Literal(1)))))
    assert(literalResult("literal")() == 1.0)
    
    val plusResult    = Calculator.computeValues(Map(("plus", Signal(Plus(Literal(1), Literal(2))))))
    assert(plusResult("plus")() == 3.0)

    val minusResult   = Calculator.computeValues(Map(("minus", Signal(Minus(Literal(1), Literal(2))))))
    assert(minusResult("minus")() == -1.0)

    val timesResult   = Calculator.computeValues(Map(("times", Signal(Times(Literal(1), Literal(2))))))
    assert(timesResult("times")() == 2.0)

    val divideResult  = Calculator.computeValues(Map(("divide", Signal(Divide(Literal(1), Literal(2))))))
    assert(divideResult("divide")() == 0.5)

    val refResult     = Calculator.computeValues(Map[String, Signal[Expr]](("ref", Signal(Ref("literal"))), ("literal", Signal(Literal(1)))))
    assert(refResult("ref")() == 1.0)
  }

  test("compute values in single map") {
    val result = Calculator.computeValues(Map(("literal", Signal(Literal(1))),
      ("plus", Signal(Plus(Literal(1), Literal(2)))),
      ("minus", Signal(Minus(Literal(1), Literal(2)))),
      ("times", Signal(Times(Literal(1), Literal(2)))),
      ("divide", Signal(Divide(Literal(1), Literal(2)))),
      ("ref", Signal(Ref("literal"))), ("literal", Signal(Literal(1)))))

    assert(result("literal")() == 1.0)
    assert(result("plus")() == 3.0)
    assert(result("minus")() == -1.0)
    assert(result("times")() == 2.0)
    assert(result("divide")() == 0.5)
    assert(result("ref")() == 1.0)
  }

  test("Potential cyclic error") {
    val result = Calculator.computeValues(Map(
      ("a", Signal(Plus(Ref("b"), Literal(1)))),
      ("b", Signal(Times(Ref("a"), Literal(2))))))

    assert(result("a")().isNaN)
    assert(result("b")().isNaN)
  }

  test("Self reference cyclic error") {
    val result = Calculator.computeValues(Map(
      ("b", Signal(Times(Ref("b"), Literal(2))))))

    assert(result("b")().isNaN)
  }
}
