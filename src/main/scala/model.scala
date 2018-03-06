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

case class PosDistanceToHero(pos: Pos, distance: Int)

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

  val taverns: List[Pos] = positionedTiles.filter(_.tile == Tavern) map { _.pos }

  def otherMinesPositions(hero: Hero): List[Pos] = {
    def loop(acc: List[Pos], pt: List[PositionedTile]): List[Pos] = pt match {
      case PositionedTile(Mine(None), pos) :: xs => loop(pos :: acc, xs)
      case PositionedTile(Mine(Some(id)), pos) :: xs if hero.id != id => loop(pos :: acc, xs)
      case x::xs => loop(acc, xs)
      case Nil => acc
    }

    loop(List(), positionedTiles)
  }

  def otherHeroesPositions(heroes: List[Hero]): List[Pos] = {
    def loop(acc: List[Pos], pt: List[PositionedTile]): List[Pos] = pt match {
      case PositionedTile(Tile.Hero(id), pos) :: xs if !heroes.exists(_.id == id) => loop(pos :: acc, xs)
      case x::xs => loop(acc, xs)
      case Nil => acc
    }

    loop(List(), positionedTiles)
  }

  def closestPositionToCurrentPos(pos: Pos, others: List[Pos]): Option[Pos] = {
    if (others.isEmpty) None

    val weightedPositions: List[(Pos, Int)] = others map { nmp: Pos =>
      (nmp, Math.abs(pos.x - nmp.x) + Math.abs(pos.y - nmp.y))
    }

    val close = others.foldRight(others.head)((p: Pos, minP: Pos) => {
        val weight = Math.abs(pos.x - p.x) + Math.abs(pos.y - p.y)
        val minPWeight = Math.abs(pos.x - minP.x) + Math.abs(pos.y - minP.y)

        if (weight < minPWeight) p else minP
    })

    Some(close)
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

case class ScoredPos(pos: Pos, g: Int, h: Int, parent: Option[ScoredPos] = None) {
  val f: Int = g + h

  val pathLength: Int = {
    def loop(path: Option[ScoredPos], length: Int): Int = path match {
      case Some(p) => loop(p.parent, length + 1)
      case _ => length
    }

    loop(Some(this), -1)
  }

  val objectivePos: ScoredPos = {
    def loop(path: ScoredPos): ScoredPos = path.parent match {
      case Some(p) => loop(p)
      case _ => path
    }

    loop(this)
  }

  val nextPos: Option[Pos] = parent map { _.pos }
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
