package bot

object Dir extends Enumeration {
  type Dir = Value
  val Stay, North, South, East, West = Value
}

import bot.Dir._
import bot.Tile.{Mine, Tavern}

case class Pos(x: Int, y: Int) {

  def neighbors = Set(North, South, West, East) map to

  def to(dir: Dir) = dir match {
    case Stay  ⇒ this
    case North ⇒ copy(x = x - 1)
    case South ⇒ copy(x = x + 1)
    case East  ⇒ copy(y = y + 1)
    case West  ⇒ copy(y = y - 1)
  }

  def isIn(size: Int) = (x >= 0 && x < size && y >= 0 && y < size)
}

sealed trait Tile
object Tile {
  case object Air extends Tile
  case object Wall extends Tile
  case object Tavern extends Tile
  case class Hero(id: Int) extends Tile
  case class Mine(heroId: Option[Int]) extends Tile
}

case class PositionedTile(tile: Tile, pos: Pos)

case class Board(size: Int, tiles: Vector[Tile]) {

  def at(pos: Pos): Option[Tile] =
    if (pos isIn size) tiles lift (pos.x * size + pos.y) else None
}

case class PositionedBoard(size: Int, positionedTiles: List[PositionedTile]) {

  def at(pos: Pos): Option[PositionedTile] =
    if (pos isIn size) positionedTiles.reverse lift (pos.x * size + pos.y) else None

  def otherMines(heroId: Int): List[Pos] = {
    def loop(acc: List[Pos], pt: List[PositionedTile]): List[Pos] = pt match {
      case PositionedTile(Mine(None), pos) :: xs => loop(pos :: acc, xs)
      case PositionedTile(Mine(Some(id)), pos) :: xs if id != heroId => loop(pos :: acc, xs)
      case x::xs => loop(acc, xs)
      case Nil => acc
    }

    loop(List(), positionedTiles)
  }

  def closestPositionToCurrentPos(pos: Pos, others: List[Pos]): Pos = {
    val weightedPositions: List[(Pos, Int)] = others map { nmp: Pos =>
      (nmp, Math.abs(pos.x - nmp.x) + Math.abs(pos.y - nmp.y))
    }

    others.foldRight(others.head)((p: Pos, minP: Pos) => {
        val weight = Math.abs(pos.x - p.x) + Math.abs(pos.y - p.y)
        val minPWeight = Math.abs(pos.x - minP.x) + Math.abs(pos.y - minP.y)

        if (weight < minPWeight) p else minP
    })
  }

  def closestTavernTo(pos: Pos): Pos = {
    val taverns: List[Pos] = positionedTiles.filter(_.tile == Tavern) map { _.pos }

    closestPositionToCurrentPos(pos, taverns)
  }

}

object PositionedBoard {
  def positionedTiles(board: Board): List[PositionedTile] =
    board.tiles.reverse.foldRight(List.empty: List[PositionedTile])((t: Tile, b: List[PositionedTile]) => {
      val previous: Option[PositionedTile] = b.headOption

      def npos(pos: Pos): Pos = {
        val n = pos.x * board.size + pos.y + 1

        val newX = n / board.size
        val newY = n % board.size

        Pos(newX, newY)
      }
      val nextPos: Pos = previous.map({ pr => npos(pr.pos)}).getOrElse(Pos(0, 0))

      PositionedTile(t, nextPos) :: b
    })
}

case class Hero(
  id: Int,
  name: String,
  pos: Pos,
  life: Int,
  gold: Int,
  mineCount: Int,
  spawnPos: Pos,
  crashed: Boolean,
  elo: Option[Int]) {

  override def toString = s"Hero $id $pos life:$life mine:$mineCount gold:$gold"
  }

case class Game(
  id: String,
  turn: Int,
  maxTurns: Int,
  heroes: List[Hero],
  board: Board,
  finished: Boolean)

case class Input(
  game: Game,
  hero: Hero,
  token: String,
  viewUrl: String,
  playUrl: String)
