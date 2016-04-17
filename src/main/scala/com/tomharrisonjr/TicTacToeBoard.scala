package com.tomharrisonjr

/**
  * Created by tharrison on 4/11/16.
  */
class TicTacToeBoard(board: Array[Array[Player]]) {

  // for tests, set up a board using strings of X's and O's
  def this(stringBoard: Array[String]) = this(stringBoard.map(row => TicTacToeBoard.getPlayersFromString(row)))

  // allows for non-square boards?
  def this(rows: Int, cols: Int) = this(Array.fill(rows, cols)(Blank): Array[Array[Player]])

  // square board
  def this(size: Int) = this(size, size)

  val rowCount = board.length
  val columnCount = if (board.isEmpty) 0 else board(0).length
  val columnNameMapping = (0 until 5 * columnCount).map(n => (TicTacToeBoard.numToAlpha(n), n)).toMap

  val numInARowNeeded = 3

  // These methods all check for winner in a game of N dimensions; if we stick with 3, it gets simpler, nay?
  def rows: Seq[Array[Player]] = {
    for (r <- 0 until rowCount)
      yield board(r)
  }

  def columns: Seq[Array[Player]] = {
    for (c <- 0 until columnCount) yield (
      for (r <- 0 until rowCount)
        yield board(r)(c)).toArray
  }

  def diagonalsLTR: Seq[Array[Player]] = {
    for (offset <- (1 - columnCount) until columnCount) yield (
      for (row <- 0 until rowCount if offset + row < columnCount && offset + row > -1)
        yield board(row)(row + offset)).toArray
  }

  def diagonalsRTL: Seq[Array[Player]] = {
    for (offset <- 0 until rowCount + rowCount - 1) yield (
      for (col <- 0 until columnCount if offset - col < rowCount && offset - col > -1)
        yield board(offset - col)(col)).toArray
  }

  def determineWinner: GameResult.Value = {
    val checkForWinner = { array: Array[Player] =>
      TicTacToeBoard.nInARow(numInARowNeeded, array) match {
        // wow, fancy and unreadable!  Yeah, if we find a winner here, return from method
        case Some(player) => return player match {
          // non-local return!
          case X => GameResult.X
          case O => GameResult.O
          // don't thing these other cases would be needed if we validated input at constructor
          case other => throw new Exception("Error, '" + other + "' is not a player.")
        }
        case None => // do nothing
      }
    }

    // if any of these finds a winner, the checkForWinner val will return.  How clever.  And unreadable.
    rows foreach checkForWinner
    columns foreach checkForWinner
    diagonalsLTR foreach checkForWinner
    diagonalsRTL foreach checkForWinner

    if (board.map(row => row.contains(Blank)).contains(true)) {
      return GameResult.NoResult
    }

    GameResult.Draw
  }

  override def toString: String = {
    var boardRepresentation = ""

    def p = { str: String => boardRepresentation = boardRepresentation.concat(str + "\n") }

    val topLine = (1 until columnCount).foldLeft("   ┌")((acc, c) => acc.concat("───┬")).concat("───┐")
    val middleLine = (0 until columnCount).foldLeft("   │")((acc, c) => acc.concat("───│"))
    val bottomLine = (1 until columnCount).foldLeft("   └")((acc, c) => acc.concat("───┴")).concat("───┘")

    p("")
    p((0 until columnCount).foldLeft("     ")((acc, n) => acc.concat("%-4s".format(TicTacToeBoard.numToAlpha(n)))))
    p(topLine)
    for (r <- 0 until rowCount) {
      var rowString = "%-3d".format(r).concat("│")
      for (c <- 0 until columnCount) {
        rowString = rowString.concat(" %s │".format(board(r)(c)))
      }
      p(rowString)
      if (r < rowCount - 1) {
        p(middleLine)
      }
    }
    p(bottomLine)
    p("")

    boardRepresentation
  }

  def validMove(row: Int, col: Int): Boolean = {
    row < rowCount && row >= 0 && col < columnCount && col >= 0 && board(row)(col) == Blank
  }

  def update(row: Int, col: Int, player: Player) = {
    board(row)(col) = player
  }

  def columnNumber(columnName: String): Int = {
    columnNameMapping(columnName)
  }
}

object TicTacToeBoard {

  def numToAlpha(number: Int): String = {
    var dividend = number + 1 // internally, treat 1 as A - just makes it easier
    var letters = ""
    var modulo = 0

    while (dividend > 0) {
      modulo = (dividend - 1) % 26
      letters = (65 + modulo).toChar + letters
      dividend = (dividend - modulo) / 26
    }

    letters
  }

  def getPlayersFromString(row: String): Array[Player] = {
    row.map(char => {
      if (char == 'X') X: Player else if (char == 'O') O: Player else Blank: Player
    }).toArray
  }

  def threeInARow(list: List[Player]): Option[Player] = list match {
    case Nil => None
    case x :: y :: z :: tail if x == y && y == z && z != Blank => Some(z)
    case _ :: tail => threeInARow(tail)
  }

  def nInARow(n: Int, array: Array[Player]): Option[Player] = {
    for (i <- 0 until array.length - (n - 1)) {
      var allTrue = true
      for (j <- i + 1 until i + n) {
        allTrue &= array(j - 1) == array(j)
      }
      if (allTrue && array(i) != Blank) {
        return Some(array(i))
      }
    }

    None
  }

}
