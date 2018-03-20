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

case class PositionedTile(tile: Tile, pos: Pos, weight: Int = 1) {
  override def toString: String = s"$tile[${pos.x}, ${pos.y}]"
}

case class Board(size: Int, tiles: Vector[Tile]) {

  def at(pos: Pos): Option[Tile] =
    if (pos isIn size) tiles lift (pos.x * size + pos.y) else None
}

case class PositionedBoard(size: Int, positionedTiles: Vector[PositionedTile]) {

  def at(pos: Pos): Option[PositionedTile] =
    if (pos isIn size) positionedTiles.reverse lift (pos.x * size + pos.y) else None

  val taverns: Vector[PositionedTile] = positionedTiles.filter(_.tile == Tavern)

  def weightedTiles(enemyHeroes: Vector[Hero]): Vector[PositionedTile] =
    enemyHeroes.foldRight(positionedTiles: Vector[PositionedTile])((a: Hero, b: Vector[PositionedTile]) => {
      val x = a.pos.neighbors.toVector flatMap (_.neighbors) distinct
      val heroNeighbors = x flatMap at

      val bWithNeighbors = heroNeighbors.foldRight(b: Vector[PositionedTile])((aa: PositionedTile, bb: Vector[PositionedTile]) => {
        bb.updated(bb.indexOf(aa), aa.copy(weight = 20))
      })

      val updated: Option[Vector[PositionedTile]] = for {
        heroTile <- at(a.pos)
        idx = bWithNeighbors.indexOf(heroTile)
      } yield bWithNeighbors.updated(idx, heroTile.copy(weight = 30))

      updated getOrElse bWithNeighbors
    })
}

object PositionedBoard {
  def positionedTiles(board: Board): Vector[PositionedTile] =
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
