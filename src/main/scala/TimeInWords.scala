object TimeInWords {

  // Complete the timeInWords function below.
  def timeInWords(h: Int, m: Int): String = {
    def unitWord(n: Int): String = {
      n match {
        case 0 => ""
        case 1 => "one"
        case 2 => "two"
        case 3 => "three"
        case 4 => "four"
        case 5 => "five"
        case 6 => "six"
        case 7 => "seven"
        case 8 => "eight"
        case 9 => "nine"
      }
    }

    def teenWord(n: Int): String = {
      n match {
        case 10 => "ten"
        case 11 => "eleven"
        case 12 => "twelve"
        case 13 => "thirteen"
        case 14 => "fourteen"
        case 15 => "fifteen"
        case 16 => "sixteen"
        case 17 => "seventeen"
        case 18 => "eighteen"
        case 19 => "nineteen"
      }
    }

    def tenWord(n: Int): String = {
      n match {
        case 20 => "twenty"
        case 30 => "thirty"
        case 40 => "fourty"
        case 50 => "fifty"
      }
    }

    def numberToWord(n: Int): String = {
      if ((1 to 9).contains(n)) {
        unitWord(n)
      } else if ((10 to 19).contains(n)) {
        teenWord(n)
      } else {
        val tenValue = (n / 10) * 10
        s"${tenWord(tenValue)} ${unitWord(n - tenValue)}".trim
      }
    }

    (h, m) match {
      case (hour, 0)  => s"${numberToWord(hour)} o' clock"
      case (hour, 1)  => s"${numberToWord(1)} minute past ${numberToWord(hour)}"
      case (hour, 15) => s"quarter past ${numberToWord(hour)}"
      case (hour, 30) => s"half past ${numberToWord(hour)}"
      case (hour, 45) => s"quarter to ${numberToWord(hour + 1)}"
      case (hour, minute) =>
        if (minute < 30)
          s"${numberToWord(minute)} minutes past ${numberToWord(hour)}"
        else
          s"${numberToWord(60 - minute)} minutes to ${numberToWord(hour + 1)}"
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val h = stdin.readLine.trim.toInt

    val m = stdin.readLine.trim.toInt

    val result = timeInWords(h, m)

    println(result)
  }
}
