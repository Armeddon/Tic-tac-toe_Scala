package board

object EndGame extends Enumeration {
  type EndGame = Value
  val WinX, WinO, Draw = Value
  def printEndGame(eg: EndGame): Unit = 
    println(eg match {
      case WinX => "Player X wins!"
      case WinO => "Player O wins!"
      case Draw => "The game ends in a draw!"
    })
}
import EndGame._

object Player extends Enumeration {
  type Player = Value
  val PlayerX, PlayerO = Value
  def printPlayer(player: Player): Unit =
    print(player match {
      case PlayerX => "Player 1"
      case PlayerO => "Player 2"
    })
  def next(player: Player): Player =
    player match {
      case PlayerO => PlayerX
      case PlayerX => PlayerO
    }
  def win(player: Player): EndGame =
    player match {
      case PlayerO => WinO
      case PlayerX => WinX
    }
}
import Player._

object Tile extends Enumeration {
  type Tile = Value
  val TileEmpty, TileX, TileO = Value
  def printTile(tl: Tile): Unit =
    print(tl match {
      case TileEmpty => "."
      case TileX => "X"
      case TileO => "O"
    })
}
import Tile._

final class Turn(val player: Player, val x: Int, val y: Int)

final class Board(val size: Int) {
  val tiles: Array[Array[Tile]] = 
    (for(i <- 0 until size) yield 
      (for(j <- 0 until size) yield
        Tile.TileEmpty
        ).toArray
      ).toArray
  def winCheck(): Option[EndGame] = {
    val rows: Option[Player] = tiles
      .map(Board.isSamePlayer(_))
      .find(_.isDefined)
      .flatten
    if (rows.isDefined)
      return rows.map(win _)

    val colomns: Option[Player] = (0 until size)
      .map(i => 
        Board.isSamePlayer(
          tiles
            .map(_(i))
            .toArray))
      .find(_.isDefined)
      .flatten
    if (colomns.isDefined)
      return colomns.map(win _)

    val diag1 = Board.isSamePlayer(
      (0 until size)
        .map(i => tiles(i)(i))
        .toArray)
    if (diag1.isDefined)
      return diag1.map(win _)

    val diag2 = Board.isSamePlayer(
      (0 until size)
        .map(i => tiles(i)(size - i - 1))
        .toArray)
    if (diag2.isDefined)
      return diag2.map(win _)
    None
  }
  def drawCheck(): Option[EndGame] =
    if (tiles.forall(_.forall(_ != TileEmpty)))
      Some(Draw)
    else
      None
  def execTurn(trn: Turn): Boolean = {
    if (!inside(trn.x) || !inside(trn.y))
      return false
    if (tiles(trn.x)(trn.y) != TileEmpty)
      return false
    tiles(trn.x)(trn.y) = trn.player match {
      case PlayerX => TileX
      case PlayerO => TileO
    }
    true
  }
  def inside(x: Int): Boolean =
    x >= 0 && x < size
}
object Board {
  def isSamePlayer(tiles: Array[Tile]): Option[Player] = {
    tiles
      .map(_ match {
        case TileX => Some(PlayerX)
        case TileO => Some(PlayerO)
        case _ => None
      })
        .reduce((t1, t2) => if (t1 == t2) t1 else None)
  }
}
