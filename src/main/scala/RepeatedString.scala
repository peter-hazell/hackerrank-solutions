object RepeatedStringSolution {

  // Complete the repeatedString function below.
  def repeatedString(s: String, n: Long): Long = {
    (s.count(_ == 'a') * (n / s.length)) + s
      .take((n % s.length).toInt)
      .count(_ == 'a')
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val s = stdin.readLine

    val n = stdin.readLine.trim.toLong

    val result = repeatedString(s, n)

    println(result)
  }
}
