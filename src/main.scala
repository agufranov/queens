object Queens {
  def generateEven(n: Int): Seq[Int] = (n % 12) match {
    case 3 | 9 => Range.inclusive(4, n, 2) :+ 2
    case _ => Range.inclusive(2, n + 1, 2)
  }

  def generateOdd(n: Int): Seq[Int] = (n % 12) match {
    case 8 => Range.inclusive(0, n / 4).flatMap(x => Seq(4 * x + 3, 4 * x + 1)).filter(_ <= n)
    case 3 | 9 => Range.inclusive(5, n, 2) ++ Seq(1, 3)
    case 2 => Seq(3, 1) ++ Range.inclusive(7, n, 2) :+ 5
    case _ => Range.inclusive(1, n, 2)
  }

  // Эвристика для построения решения
  def generate(n: Int): Seq[Int] = (generateEven(n) ++ generateOdd(n)) map(_ - 1)

  def permute(xs: Seq[Int]): Option[Seq[Int]] = {
    val _i = Range.inclusive(xs.length - 2, 0, -1).find(i => xs(i) < xs(i + 1))
    _i match {
      case Some(i) =>
        val pi = xs(i)
        val (pj, j) = xs.zipWithIndex.drop(i).filter(x => pi < x._1).minBy(x => x._1)
        val r = xs.updated(i, pj).updated(j, pi)
        // println("r:", r, r.drop(j + 1))
        Some(r.take(i + 1) ++ r.drop(i + 1).reverse)
      case _ => None
    }
  }

  def isDistinct(xs: Seq[Int]) = xs.length == xs.distinct.length

  // Проверяет, есть ли в позиции xs бой по диагонали
  def checkDiags(xs: Seq[Int]): Boolean = {
    val symDiags = xs.zipWithIndex.map(x => x._1 - x._2)
    val scewDiags = xs.zipWithIndex.map(x => x._1 + x._2)
    isDistinct(symDiags) && isDistinct(scewDiags)
  }

  def main(args: Array[String]) {
    val runtime = Runtime.getRuntime
    println("Enter n:")
    val n = io.StdIn.readLine.toInt
    println("Calculating...")
    // Используем эвристику для построения "базового" рещения
    var solution = Option(generate(n))
    val solutions = collection.mutable.ArrayBuffer[Seq[Int]]()
    var outOfMem = false
    try {
      while(solution != None) {
        if(runtime.freeMemory < n * 4) {
          throw OutOfMem
        }
        if(checkDiags(solution.get)) {
          solutions += solution.get
        }
        solution = permute(solution.get)
      }
    }
    catch {
      case OutOfMem => println(s"Out of memory. Solutions found: ${solutions.length}")
      return
    }
    println(s"Done. Solutions found: ${solutions.length}")
  }

  object OutOfMem extends Exception {}
}
