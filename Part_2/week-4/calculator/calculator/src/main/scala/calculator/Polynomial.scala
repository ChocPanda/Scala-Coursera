package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    // Δ = b² - 4ac
    Signal {
      val sampleB = b()
      (sampleB * sampleB) - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / (2a)
    Signal {
      val sampleB = b()
      val sampleDelta = delta()
      val divisor = 2 * a()
      if (sampleDelta < 0) Set.empty
      else Set(
      (-sampleB + Math.sqrt(sampleDelta)) / divisor,
      (-sampleB - Math.sqrt(sampleDelta)) / divisor)
    }
  }
}
