import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._
import scala.util.{Success, Try}

object Result {

  /*
   * Complete the 'jumpingOnClouds' function below.
   *
   * The function is expected to return an INTEGER.
   * The function accepts INTEGER_ARRAY c as parameter.
   */

  def jumpingOnClouds(c: Array[Int]): Int = {
    def jump(currentPosition: Int = 0, jumpCount: Int = 0): Int = {
      def trySingleJump: Int = {
        Try(c(currentPosition + 1)) match {
          case Success(0) => jump(currentPosition + 1, jumpCount + 1)
          case _ => jumpCount
        }
      }

      Try(c(currentPosition + 2)) match {
        case Success(0) => jump(currentPosition + 2, jumpCount + 1)
        case Success(1) => trySingleJump
        case _ => trySingleJump
      }
    }

    jump()
  }

}

object Solution {
  def main(args: Array[String]) {
    val c = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)

    val result = Result.jumpingOnClouds(c)

    println(result)
  }
}
