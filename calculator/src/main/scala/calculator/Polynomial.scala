package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
    c: Signal[Double]): Signal[Double] = Signal {
    Math.pow(b(), 2) - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
    c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val Δ = computeDelta(a, b, c)()

    if (Δ == 0) Set(-b() / (2 * a()))
    else if (Δ > 0) Set((-b() + Math.sqrt(Δ)) / (2 * a()), (-b() - Math.sqrt(Δ)) / (2 * a()))
    else Set.empty[Double]
  }
}
