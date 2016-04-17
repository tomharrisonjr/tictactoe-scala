package com.tomharrisonjr

import org.scalatest._

/**
  * Created by tharrison on 4/10/16.
  */
class TicTacToeBoard$Test extends FunSuite {

  test("testDetermineWinner") {
    // Testing win detection
    assert(new TicTacToeBoard(Array(
      "XOX",
      "XOO",
      "XXO"
    )).determineWinner == GameResult.X)

    assert(new TicTacToeBoard(Array(
      "XOX",
      "OOO",
      " XO"
    )).determineWinner == GameResult.O)

    assert(new TicTacToeBoard(Array(
      "XOX",
      "XOO",
      " XO"
    )).determineWinner == GameResult.NoResult)

    assert(new TicTacToeBoard(Array(
      "XOX",
      "XOO",
      "OXO"
    )).determineWinner == GameResult.Draw)

    assert(new TicTacToeBoard(Array(
      "XXO",
      "XOO",
      "OXX"
    )).determineWinner == GameResult.O)

    assert(new TicTacToeBoard(Array(
      "XXO",
      "XXO",
      "OOX"
    )).determineWinner == GameResult.X)
  }

}
