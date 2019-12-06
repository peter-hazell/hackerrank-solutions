object Staircase {

  // Complete the staircase function below.
  def staircase(n: Int): Unit = {
    (1 to n).foreach(x => println(s"${" " * (n - x)}${"#" * x}"))
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val n = stdin.readLine.trim.toInt

    staircase(n)
  }
}
