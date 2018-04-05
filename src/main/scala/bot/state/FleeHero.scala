package bot.state

import bot.GameEngine.GameContext
import bot.Tile.{Air, Tavern}
import bot._
import bot.pathfind.{Path, Pathfinder}

class FleeHero(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder
  val enemyHeroes: Vector[Hero] = context.enemyHeroes

  override def run(): Path = {
    val enemyHeroesTiles: Vector[PositionedTile] = enemyHeroes collect {
      case enemy if isEnemyStrong(enemy) => board at enemy.pos get
    }

    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val path = pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)
    val fleeTile: Option[PositionedTile] = for {
      current <- path.current
      destination <- path.destination
    } yield fleeingTile(current, destination)

    Path(Vector(path.current, fleeTile).flatten)
  }

  override def validate(): ValidReason = {
    // filter tiles for enemy heroes that have at least 1 mine and their life is at least 21 HP less than ours
    val enemyHeroesTiles: Vector[PositionedTile] = enemyHeroes collect {
      case enemy if isEnemyStrong(enemy) => board at enemy.pos get
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val closestTavernPath: Path = pathfinder.multiGoalFind(board.taverns, heroTile)
    val path = pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)

    if (closestTavernPath.length == 1 && hero.life < 91) ValidReason(valid = false)
    else if (path.isNonEmpty && path.length < 3) ValidReason(valid = true, s"Fleeing, as enemy has at least 20 more life than us and is ${path.length} tiles away")
    else ValidReason(valid = false)
  }

  private def isEnemyStrong(enemy: Hero): Boolean = hero.life - enemy.life < 20

  override def toString: String = "Flee Hero"

  private def fleeingTile(current: PositionedTile, enemy: PositionedTile): PositionedTile = {
    current.pos.neighbors.find(board.at(_).exists(_.tile == Tavern)) match {
      case Some(pos) => PositionedTile(Tavern, pos)
      case None =>
        val fleeRoutes: Vector[Pos] = current.pos.neighbors.toVector.filter(p => board.at(p).exists(_.tile == Air))
        val fleePositionedTiles: Vector[PositionedTile] = fleeRoutes flatMap board.at
        val fleePaths: Vector[Path] = fleePositionedTiles map { f => pathfinder.multiGoalFind(Vector(enemy), f) }

        maxPath(fleePaths) flatMap (_.current) getOrElse current
    }
  }

  private def maxPath(paths: Vector[Path]): Option[Path] = paths match {
    case x +: y +: xs => if (x.length >= y.length) maxPath(x +: xs) else maxPath(y +: xs)
    case x +: _ => Some(x)
    case _ => None
  }
}