import java.io._

object CountingValleys {

  // Complete the countingValleys function below.
  def countingValleys(n: Int, s: String): Int = {
    s.foldLeft((0: Int, 0: Int)) {
        case ((position, valleyCount), c) =>
          c match {
            case 'U' => (position + 1, valleyCount)
            case 'D' => {
              if (position == 0) (position - 1, valleyCount + 1)
              else (position - 1, valleyCount)
            }
          }
      }
      ._2
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val s = stdin.readLine

    val result = countingValleys(n, s)

    printWriter.println(result)

    printWriter.close()
  }
}
