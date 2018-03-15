package bot.state

import bot.Tile.Mine
import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekHero(implicit val hero: Hero, implicit val board: PositionedBoard, implicit val pathfinder: Pathfinder) extends Behavior {
  override def run(): Path = {
    val enemyHeroesTiles: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Hero(id), pos) if id != hero.id => board at pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)

    pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)
  }

  override def validate(): ValidReason = {
    // filter tiles for enemy heroes that have at least 1 mine
    val enemyHeroesTiles: Vector[PositionedTile] = board.positionedTiles collect {
      case PositionedTile(Tile.Hero(id), pos) if id != hero.id && mines.exists(_.tile == Mine(Some(id))) => board at pos get
    }

    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val path = pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)

    if (path.length < 3) ValidReason(valid = true, s"Hero is ${path.length} close.")
    else ValidReason(valid = false)
  }

  private val mines: Vector[PositionedTile] = board.positionedTiles collect {
    case PositionedTile(Tile.Mine(owner), pos) if owner.isEmpty || owner.exists( hero.id !=) => board at pos get
  }

  override def toString: String = "Seek Hero"
}