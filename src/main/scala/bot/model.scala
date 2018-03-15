package bot

object Dir extends Enumeration {
  type Dir = Value
  val Stay, North, South, East, West = Value
}


import bot.Dir.{Dir, _}
import bot.Tile.Tavern

case class DirReason(dir: Dir, reason: String = "")
case class ValidReason(valid: Boolean, reason: String = "")
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

case class PositionedTile(tile: Tile, pos: Pos, neighbors: Vector[PositionedTile] = Vector.empty) {
  override def toString: String = s"$tile[${pos.x}, ${pos.y}] ${neighbors.length} neighbors"
}

case class Board(size: Int, tiles: Vector[Tile]) {

  def at(pos: Pos): Option[Tile] =
    if (pos isIn size) tiles lift (pos.x * size + pos.y) else None
}

case class PositionedBoard(size: Int, positionedTiles: Vector[PositionedTile]) {

  def at(pos: Pos): Option[PositionedTile] =
    if (pos isIn size) positionedTiles.reverse lift (pos.x * size + pos.y) else None

  val taverns: Vector[PositionedTile] = positionedTiles.filter(_.tile == Tavern)

//  def otherMinesPositions(heroes: List[Hero]): List[Pos] = {
//    def loop(acc: List[Pos], pt: List[PositionedTile]): List[Pos] = pt match {
//      case PositionedTile(Mine(None), pos) :: xs => loop(pos :: acc, xs)
//      case PositionedTile(Mine(Some(id)), pos) :: xs if heroes.exists(_.id == id) => loop(pos :: acc, xs)
//      case x::xs => loop(acc, xs)
//      case Nil => acc
//    }
//
//    loop(List(), positionedTiles)
//  }

}

object PositionedBoard {
  def positionedTiles(board: Board): Vector[PositionedTile] = {
    val tilesWithoutNeighbors: Vector[PositionedTile] =
      board.tiles.foldLeft(Vector.empty: Vector[PositionedTile])((b: Vector[PositionedTile], t: Tile) => {
        val previous: Option[PositionedTile] = b.headOption

        def npos(pos: Pos): Pos = {
          val n = pos.x * board.size + pos.y + 1

          val newX = n / board.size
          val newY = n % board.size

          Pos(newX, newY)
        }

        val nextPos: Pos = previous.map({ pr => npos(pr.pos) }).getOrElse(Pos(0, 0))

        PositionedTile(t, nextPos) +: b
      })


    val positionedBoardWithoutNeighbors: PositionedBoard = PositionedBoard(board.size, tilesWithoutNeighbors)

    val positionedBoardWithNeighbors: Vector[PositionedTile] = tilesWithoutNeighbors map { twn =>
      val neighbors: Vector[PositionedTile] = twn.pos.neighbors.toVector.flatMap(positionedBoardWithoutNeighbors.at)

      PositionedTile(twn.tile, twn.pos, neighbors)
    }

    val tmpPositionedBoard = PositionedBoard(board.size, positionedBoardWithNeighbors)

    positionedBoardWithNeighbors map { tn => tn.
      PositionedTile(tn.tile, tn.pos, tn.neighbors.flatMap(n => tmpPositionedBoard.at(n.pos)))
    }
  }
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
