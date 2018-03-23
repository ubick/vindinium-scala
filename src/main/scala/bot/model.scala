package bot

object Dir extends Enumeration {
  type Dir = Value
  val Stay, North, South, East, West = Value
}


import bot.Dir.{Dir, _}
import bot.Tile.{Air, Mine, Tavern}
import bot.pathfind.Path

case class DirReason(dir: Dir, reason: String = "", path: Option[Path] = None)
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
  val taverns: Vector[PositionedTile] = positionedTiles.filter(_.tile == Tavern)

  def at(pos: Pos): Option[PositionedTile] =
    if (pos isIn size) positionedTiles.reverse lift (pos.x * size + pos.y) else None

  val mines: Vector[PositionedTile] = positionedTiles.filter(_.tile.isInstanceOf[Mine])

  def otherMines(hero: Hero): Vector[PositionedTile] = mines.filterNot(_.tile == Mine(Some(hero.id)))

  def weightedTiles(enemyHeroes: Vector[Hero], hero: Hero): Vector[PositionedTile] = {
    // only add weight around stronger heroes
    val enemyTiles: Vector[PositionedTile] = enemyHeroes collect {
      case h: Hero if hero.life - h.life <= 20 => h.pos
    } flatMap at

    enemyTiles.foldRight(positionedTiles: Vector[PositionedTile])((a: PositionedTile, b: Vector[PositionedTile]) => {
      val neighbors: Vector[PositionedTile] = withNeighbors(a, b, None)

      val updated: Option[Vector[PositionedTile]] = for {
        heroTile <- at(a.pos)
        idx = neighbors.indexOf(heroTile)
      } yield neighbors.updated(idx, heroTile.copy(weight = 40))

      updated getOrElse neighbors
    })
  }

  private def withNeighbors(source: PositionedTile, tiles: Vector[PositionedTile], parent: Option[PositionedTile]): Vector[PositionedTile] = {
    def loop(source: PositionedTile, tiles: Vector[PositionedTile], parent: Option[PositionedTile], level: Int): Vector[PositionedTile] = {
      if (level > 3) tiles
      else {
        val neighbors: Vector[PositionedTile] = source.pos.neighbors.toVector flatMap at filter (n => n.tile == Air && !parent.exists(_.pos == n.pos))

        val tilesWithNeighbors: Vector[PositionedTile] = neighbors.foldRight(tiles: Vector[PositionedTile])((aa: PositionedTile, bb: Vector[PositionedTile]) => {
          if (bb.indexOf(aa) != -1) bb.updated(bb.indexOf(aa), aa.copy(weight = 40 - level * 10))
          else bb
        })

        neighbors.foldRight(tilesWithNeighbors)((a: PositionedTile, b: Vector[PositionedTile]) => {
          loop(a, b, Some(source), level + 1)
        })
      }
    }

    loop(source, tiles, parent, 1)
  }
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
