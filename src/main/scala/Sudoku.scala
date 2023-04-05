/*
 * CS3210 - Principles of Programming Languages - Fall 2022
 * Instructor: Thyago Mota
 * Description: Prg 02 - Sudoku Puzzle
 * Student(s) Name(s): Jose W. Garcia-Martinez and Emily Carrillo
 */

import scala.collection.mutable
import scala.io._

object Sudoku {
  // TODOw #1: return an 2D array of Int representing a sudoku board given a filename
  def readBoard(fileName: String): Array[Array[Int]] = {
    var twoDimArray = Source.fromFile(fileName).getLines().toArray.map(x=> x.split("").map(_.toInt))
    twoDimArray
  }
  // TODOw #2: return a String representation from a given sudoku board
  def boardToString(board: Array[Array[Int]]): String = {
    val stringArray = board.map("[" + _.mkString(",") + "]").mkString(",\n")
    stringArray
  }
  // TODOw #3: return a specific row from a sudoku board as a sequence of numbers
  def getRow(board: Array[Array[Int]], row: Int): Array[Int] = {
    val rowArray = board(row)
    rowArray
  }
  // TODOw #4: return a specific column from a sudoku board as a sequence of numbers
  def getCol(board: Array[Array[Int]], col: Int): Array[Int] = {
    /**
     * the transpose of a matrix is an operator which flips a matrix over its diagonal;
     * that is, it switches the row and column indices of the matrix A
     * by producing another matrix, often denoted by Aᵀ
     * */
    val colArray = board.transpose
    return colArray(col)
  }

  // TODOw #5: return a specific box from a sudoku board as a sequence of numbers
  def getBox(board: Array[Array[Int]], x: Int, y: Int): Array[Int] = {
    val array = {
      for {
        i <- (3 * x) to (3 * x + 2)
        j <- (3 * y) to (3 * y + 2)
      } yield board(i)(j)
    }.toArray
    array
  }

  // TODOw #6: a sequence is valid if it has 9 numbers in [0-9] with possibly repeated zeros
  /*
    def getNonRepeat(seq: Array[Int]): Array[Int] = {
    filter takes out every number that is not a zero
    var nonZeros = seq.filter(_ != 0)
    .distinct.length gives the length of the sequence without the repetitions if there are any\
    true if the sequence.distinct == nonzero.length. Meaning not repetition were found in the first sequence
    nonZeros.distinct.length == nonZeros.length
    nonZeros
  }*/
  def isValid(seq: Array[Int]): Boolean = {
    val nonZeros = seq.filter(_ != 0)
    nonZeros.distinct.length == nonZeros.length
  }
  // TODOw #7: return whether all rows of the given board are valid sequences
 def allRowsValid(board: Array[Array[Int]]): Boolean = {
   val bool = {
     for {
       i <- 0 to 8
       if (isValid(getRow(board, i)))
     } yield 1

   }
    bool.sum == 9
 }
  // TODOw #8: return whether all columns of the given board are valid sequences
  def allColsValid(board: Array[Array[Int]]): Boolean = {
    val bool = {
      for {
        i <- 0 to 8
        if (isValid(getCol(board, i)))
      } yield 1

    }
    bool.sum == 9
  }
  // TODOw #9: return whether all boxes of the given board are valid sequences
  def allBoxesValid(board: Array[Array[Int]]): Boolean = {
     (isValid(getBox(board,0,0)) && isValid(getBox(board,0,1)) && isValid(getBox(board,0,2)) &&
      isValid(getBox(board,1,0)) && isValid(getBox(board,1,1)) && isValid(getBox(board,1,2)) &&
      isValid(getBox(board,2,0)) && isValid(getBox(board,2,1)) && isValid(getBox(board,2,2)))
  }
  // TODOw #10: a board is valid if all of its rows, columns, and boxes are also valid
  def isValid(board: Array[Array[Int]]): Boolean = {
    allRowsValid(board) && allColsValid(board) && allBoxesValid(board)
  }
  // TODOw #11: a board is complete it there is no zero
  def isComplete(board: Array[Array[Int]]): Boolean = {
    board.filter(_.filter(_ > 0).length == board.length).length == board.length
  }
  // TODOw #12: a board is solved if is complete and valid
  def isSolved(board: Array[Array[Int]]): Boolean = {
    isComplete(board) && isValid(board)
  }
  // TODOw #13: return a new board configuration from the given one by setting a digit at a specific (row, col) location
  def getChoice(board: Array[Array[Int]], row: Int, col: Int, d: Int): Array[Array[Int]] = {
    val newChoice = board.map(_.map(identity))
    newChoice(row)(col) = d
    newChoice
  }
  // TODOw #14: return all possible new board configurations from the given one
  def getChoices(board: Array[Array[Int]]): IndexedSeq[Array[Array[Int]]] = {
      for {
        i <- 0 to 8
        j <- 0 to 8
        d <- 1 to 9
        if(board(i)(j) == 0 && isValid(getChoice(board,i,j,d)))
      } yield getChoice(board, i, j, d)
  }
  // TODOw #15: return a solution to the puzzle (null if there is no solution)
  def solve(board: Array[Array[Int]]): Array[Array[Int]] = {
    val validBoards = getChoices(board)
    if(isSolved(board))
      return board
    else validBoards.foreach(x => return solve(x))
      null
  }
  /**
   * @Params Board: used to test every function.
   * */
  def test(board: Array[Array[Int]])= {
    print("\n\n" + boardToString(board)) // BoardToString

  }

  def main(args: Array[String]): Unit = {
    val board = readBoard("sudoku1.txt")
    val sol = solve(board)
    println(boardToString(sol))

   // test(board)
  }
}
