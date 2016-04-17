package com.tomharrisonjr

import java.io.IOException

import scala.io.StdIn

/**
  * Created by tharrison on 4/11/16.
  */
object Game {
  val Position = """([A-Za-z]+)\s*(\d+)""".r

  // pretty sure that if Game extends App mail method would be implicit
  def main(args: Array[String]) {
    val size = readBoardSize
    val board = new TicTacToeBoard(size)
    println("\n" + board.numInARowNeeded + " in a row to win (" + size + "x" + size + " board)")

    var player: Player = X
    while (board.determineWinner == GameResult.NoResult) {
      println(board)
      println("Player %s's turn.".format(player))

      val (row, col) = readNextMove(board)
      board.update(row, col, player)

      player = if (player == X) O else X
    }

    println(board)
    println(GameResult.displayGameResult(board.determineWinner))
    println
  }

  def readBoardSize: Int = {
    3
  }

  def readNextMove(board: TicTacToeBoard): (Int, Int) = {
    var validMove = false
    var col = -1
    var row = -1
    while (!validMove) {
      var input = ""
      try {
        print("Enter square: (e.g. A0): ")
        input = StdIn.readLine
        val Position(columnName, rowNumber) = input
        row = rowNumber.toInt
        col = board.columnNumber(columnName.toUpperCase)
      } catch {
        case _: IOException => println("Error reading input: Could not understand \"" + input + "\"")
      }

      validMove = board.validMove(row, col)
      if (!validMove) {
        println("Can't move there, try again!\n")
      }
    }

    (row, col)
  }
}
