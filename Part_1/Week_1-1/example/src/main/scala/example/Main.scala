package example

import example.Lists._

object Main extends App {
  println("max(List(1, 45, 32, 45, 76, 4)) = " + max(List(1, 45, 32, 45, 26, 4)))
  println("sum(List(1, 45, 32, 45, 76, 4)) = " + sum(List(1, 45, 32, 45, 76, 4)))
}
