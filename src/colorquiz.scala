package lx07

import scala.collection.mutable.ArraySeq
import scala.io.StdIn
import scala.util.Random

object ColorQuiz {
  val debug = true
  val endless = false

  val colornames = ("黒,白,赤,青,緑,金,桃,橙,紫,藍").split(",").toList

  // 状態変数
  type Answer = List[Int]
  var correctAnswer: Answer = List(0, 0)
  var trial = 0

  def master() {
    correctAnswer = List.fill(2)(Random.nextInt(10))
    if (debug) println(correctAnswer.map((i) => colornames(i)))
    trial = 0
  }

  def do_check(ca: Answer, a: Answer): (String, Boolean) = {
    if (a == ca) ("おみごと！", true)
    else {
      (if (a(0) == ca(0) || a(1) == ca(1)) "一箇所正解"
        else if (a(0) == ca(1) || a(1) == ca(0)) "いずれかの色を使ってます"
        else "大はずれ",
        false)
    }
  }

  def master_check(answer: List[Int]): Boolean = {
    do_check(correctAnswer, answer) match {
      case (message, finished) =>
        print(f"${trial}回目の試行: $message")
        finished
    }
  }

  /**
   * 入力を受け取り，コンマや空白を区切り文字として扱う方法のサンプル
   *
   * 実行時にエラーが出る場合は Evernote の「注意」を参照のこと
   */
  def input() : List[Int] = {
    println("以下から2色を選んで入力して下さい．")
    println(colornames.zip(Range(0, colornames.length, 1))
      .map((ci: (String, Int)) =>
          ci match { case (c, i) => i.toString + ":" + c})
      .mkString(", "))

    // 空白またはコンマ [ ,] の連続 [ ,]+ を区切りとして入力行を分割
    // "正規表現の記述".r というように正規表現を文字列で記述し .r を付加することで正規表現が得られる
    "[ ,]+".r.split(StdIn.readLine()) match {
      case Array(s1, s2) => List(s1.toInt, s2.toInt)
    }
  }

  def main(arguments: Array[String]) {
    master()
    do { trial = trial + 1 } while (!(master_check(input())))
    if (endless) main(arguments)
  }
}
