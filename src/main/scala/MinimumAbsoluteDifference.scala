object MinimumAbsoluteDifferenceSolution {

  // Complete the minimumAbsoluteDifference function below.
  def minimumAbsoluteDifference(arr: Array[Int]): Int = {
    arr.toList.sorted.sliding(2).map(l => l.max - l.min).min
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = minimumAbsoluteDifference(arr)

    println(result)
  }
}
