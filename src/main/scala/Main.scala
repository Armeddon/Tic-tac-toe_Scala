package main

import scala.io.StdIn
import scala.annotation.tailrec

import board.{Player, Board, Tile, EndGame, Turn}

object Game {
  def main(args: Array[String]): Unit = {
    var board = Game.getBoard()
    makeTurn(board, Player.PlayerX)
  }
  @tailrec
  def getBoard(): Board = {
    print("Choose board size: ")
    val input = StdIn.readLine().toIntOption
    input match {
      case Some(n) => new Board(n)
      case None => {
        println("Bad format!")
        getBoard()
      }
    }
  }
  @tailrec
  def makeTurn(board: Board, player: Player.Player): Unit = {
    board.drawCheck match {
      case Some(draw) => return endGame(board, draw);
      case _ => 
    }
    printBoard(board)
    val turn = getTurn(player)
    val status = board.execTurn(turn)
    if (status) {
      board.winCheck match {
        case Some(eg) => endGame(board, eg);
        case None => makeTurn(board, Player.next(player));
      }
    } else {
      println("Can't move there!")
      makeTurn(board, player)
    }
  }
  def endGame(board: Board, eg: EndGame.EndGame): Unit = {
    printBoard(board)
    EndGame.printEndGame(eg)
    System.exit(0)
  }
  def printBoard(board: Board): Unit = {
    board.tiles.foreach(line => {
      line.foreach(tile => {
        Tile.printTile(tile)
        print(" ")
      })
      println("")
    })
  }
  @tailrec
  def getTurn(player: Player.Player): Turn = {
    Player.printPlayer(player)
    println(": take your turn!")
    val input = StdIn.readLine().split(' ')
    if (input.head != ' ' && input.last != ' ' && input.length == 2) {
      if (input.head.toIntOption.isDefined && input.last.toIntOption.isDefined)
        new Turn(player, input.head.toInt - 1, input.last.toInt - 1)
      else {
        println("Not a number!")
        getTurn(player)
      }
    } else {
      println("Incorrect tile format!")
      return getTurn(player)
    }
  }
}
