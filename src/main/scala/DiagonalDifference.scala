import scala.io.StdIn

object DiagonalDifference {
  /*
   * Complete the 'diagonalDifference' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts 2D_INTEGER_ARRAY arr as parameter.
   */

  def diagonalDifference(arr: Array[Array[Int]]): Int = {
    val f = arr.flatten.toList

    val leftInc = arr.length + 1
    val leftToRight = f
      .foldLeft((0: Int, 0: Int, 0: Int)) {
        case ((accum, nextPos, index), n) => {
          if (nextPos == 0 || nextPos == index) {
            (accum + n, nextPos + leftInc, index + 1)
          } else (accum, nextPos, index + 1)
        }
      }
      ._1

    val rightDec = arr.length - 1
    val rightToLeft =
      f.foldLeft((0: Int, rightDec: Int, 0: Int)) {
          case ((accum, nextPos, index), n) => {
            if (nextPos == index && nextPos != f.size - 1) {
              (accum + n, nextPos + rightDec, index + 1)
            } else (accum, nextPos, index + 1)
          }
        }
        ._1

    rightToLeft.max(leftToRight) - rightToLeft.min(leftToRight)
  }

  def main(args: Array[String]) {
    val n = StdIn.readLine.trim.toInt

    val arr = Array.ofDim[Int](n, n)

    for (i <- 0 until n) {
      arr(i) =
        StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    }

    val result = diagonalDifference(arr)

    println(result)
  }
}
