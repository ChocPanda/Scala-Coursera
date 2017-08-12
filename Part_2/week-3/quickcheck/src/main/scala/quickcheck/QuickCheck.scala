package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[Int]
      h <- oneOf[H](empty, insert(v, empty))
    } yield h
  )

  lazy val genPair: Gen[(Int, Int)] =
    for {
      v1 <- arbitrary[Int]
      v2 <- arbitrary[Int]
    } yield (v1, v2)

  lazy val genTriple: Gen[(Int, Int, Int)] =
    for {
      v1 <- arbitrary[Int]
      v2 <- arbitrary[Int]
      v3 <- arbitrary[Int]
    } yield (v1, v2, v3)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  implicit lazy val arbTriple: Arbitrary[(Int, Int, Int)] = Arbitrary(genTriple)
  implicit lazy val arbPair: Arbitrary[(Int, Int)] = Arbitrary(genPair)

  property("gen1") = forAll { (pair: (Int, Int)) =>
    findMin(insert(pair._1, insert(pair._2, empty))) == List(pair._1, pair._2).min
  }

  property("gen2") = forAll { (triple: (Int, Int, Int)) =>
    List(insert(triple._1, insert(triple._2, insert(triple._3, empty))),
    insert(triple._1, insert(triple._3, insert(triple._2, empty))),
    insert(triple._2, insert(triple._1, insert(triple._3, empty))),
    insert(triple._2, insert(triple._3, insert(triple._1, empty))),
    insert(triple._3, insert(triple._2, insert(triple._1, empty))),
    insert(triple._3, insert(triple._1, insert(triple._2, empty)))).map(findMin).distinct.size == 1
  }

  property("gen3") = forAll { (triple: (Int, Int, Int)) =>
    List(insert(triple._1, insert(triple._2, insert(triple._3, empty))),
      insert(triple._1, insert(triple._3, insert(triple._2, empty))),
      insert(triple._2, insert(triple._1, insert(triple._3, empty))),
      insert(triple._2, insert(triple._3, insert(triple._1, empty))),
      insert(triple._3, insert(triple._2, insert(triple._1, empty))),
      insert(triple._3, insert(triple._1, insert(triple._2, empty)))).map(deleteMin).map(findMin).distinct.size == 1
  }
}
