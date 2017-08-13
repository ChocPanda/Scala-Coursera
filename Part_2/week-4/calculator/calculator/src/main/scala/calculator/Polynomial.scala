package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    // Δ = b² - 4ac
    val sampleB = b()
    Signal((sampleB * sampleB) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / (2a)
    val sampleB = b()
    val sampleDelta = delta()
    val divisor = 2 * a()
    Signal {
      if (sampleDelta < 0) Set.empty
      else Set(
      (-sampleB + Math.sqrt(sampleDelta)) / divisor,
      (-sampleB - Math.sqrt(sampleDelta)) / divisor)
    }
  }
}
