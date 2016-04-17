package com.tom

/**
  * Created by tharrison on 4/11/16.
  */
object GameResult extends Enumeration {
  type GameResult = Value
  val X, O, Draw, NoResult = Value

  def displayGameResult(gameResult: GameResult): String = {
    val winnerText = "Player %s won!"

    gameResult match {
      case GameResult.NoResult => "No winner yet!"
      case GameResult.Draw => "It's a draw.  Harrumph."
      case player => winnerText.format(player)
    }
  }
}
