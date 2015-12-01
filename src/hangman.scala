package lx07

import scala.collection.mutable.ArraySeq
import scala.util.Random
import scala.io.StdIn

object Hangman {
  val debug = true
  val endless = false

  type Word = Set[Char]

  val r = new Random

  val dictionary : ArraySeq[Word] = "hello,world,is,a,stupid,program,and,should,never,be,used,okay".split(",").map(word => word.toSet)

  val bodyParts : List[String] = "head,body,left arm,right arm,left leg,right leg".split(",").toList

  /**
   * まだ描画されずに残っている体躯の部分
   */
  var bodyPartsLeft: List[String] = bodyParts

  /**
   * 親がゲームのために選択した単語．子はこの単語を言い当てなくてはいけない．
   */
  var chosenWord: Word = dictionary(0)

  /**
   * 親が選んだ単語について子がすでに得ている知識
   */
  var knownLetters: Word = Set()

  def hangman() {
    chosenWord = dictionary(r.nextInt(dictionary.length))
    knownLetters = Set()
    bodyPartsLeft = bodyParts
  }

  def hangmanCheck(correctAnswer: Word, knowledge: Word, bodyPartsLeft: List[String], c: Char) = {
    val newKnowledge = (knowledge + c).intersect(correctAnswer)
    val youWon = newKnowledge == correctAnswer
    val badGuess = newKnowledge.size == knowledge.size
    val gameOver = badGuess && bodyPartsLeft.length == 1
    val shouldContinue = !(youWon || gameOver)
    val message =
      (if (youWon) "You won!"
        else if (!badGuess) "Good guess"
        else if (gameOver) "The End"
        else "Bad guess: " + bodyPartsLeft.head)
    (newKnowledge, message, badGuess, shouldContinue)
  }

  def hangmanGuess(c: Char): Boolean = {
    if (debug) println(f"Your guess: $c")
    hangmanCheck(chosenWord, knownLetters, bodyPartsLeft, c) match {
      case (knownLetters2, message, badGuess, shouldContinue) => {
        knownLetters = knownLetters2
        if (debug) println(knownLetters)
        println(message)
        if (badGuess) bodyPartsLeft = bodyPartsLeft.tail
        shouldContinue
      }
    }
  }

  def inputChar() = {
    println(); println("Guess a letter")
    val c = StdIn.readChar()
    c
  }

  def main(arguments: Array[String]) {
    do {} while (hangmanGuess(inputChar()))
  }
}
