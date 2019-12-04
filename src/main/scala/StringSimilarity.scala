import scala.annotation.tailrec
object StringSimilarity {

  // Complete the stringSimilarity function below.
  def stringSimilarity(s: String): Int = {
    def suffixes: List[String] = {
      @tailrec
      def get(l: List[Char], suffixes: List[String], n: Int): List[String] = {
        l match {
          case Nil => suffixes
          case x :: tail =>
            get(tail, suffixes :+ l.takeRight(n).mkString, n - 1)
        }
      }

      get(s.toList, List.empty[String], s.length)
    }

    val similarityTotal = suffixes
      .map(
        (suffix: String) =>
          suffix
            .foldLeft(0: Int, 0: Int, false: Boolean) {
              case ((accum: Int, pos: Int, stop: Boolean), suffixChar) =>
                if (s(pos) == suffixChar && !stop)
                  (accum + 1, pos + 1, false: Boolean)
                else (accum, pos + 1, true: Boolean)
            }
            ._1
      )
      .sum

    similarityTotal
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val t = stdin.readLine.trim.toInt

    for (tItr <- 1 to t) {
      val s = stdin.readLine

      val result = stringSimilarity(s)

      println(result)
    }
  }
}
