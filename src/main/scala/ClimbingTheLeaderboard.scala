object ClimbingTheLeaderboard {

  // Complete the climbingLeaderboard function below.
  def climbingLeaderboard(scores: Array[Int], alice: Array[Int]): Array[Int] = {
    alice.map { aliceScore =>
      scores
        .foldLeft(1: Int, 0: Int) {
          case ((ranking, prevScore), score) =>
            if (aliceScore >= score || score == prevScore) {
              (ranking, score)
            } else {
              (ranking + 1, score)
            }
        }
        ._1
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val scoresCount = stdin.readLine.trim.toInt

    val scores = stdin.readLine.split(" ").map(_.trim.toInt)
    val aliceCount = stdin.readLine.trim.toInt

    val alice = stdin.readLine.split(" ").map(_.trim.toInt)
    val result = climbingLeaderboard(scores, alice)

    println(result.mkString("\n"))
  }
}
