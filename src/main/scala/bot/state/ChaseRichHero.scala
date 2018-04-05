package bot.state

import bot.GameEngine.GameContext
import bot.Tile.Tavern
import bot._
import bot.pathfind.{Path, Pathfinder}

class ChaseRichHero(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder
  val enemyHeroes: Vector[Hero] = context.enemyHeroes
  val richThreshold: Int = board.mines.length / 3
  val richHero: Option[Hero] = enemyHeroes.find(_.mineCount > richThreshold)
  val closestTavernPath: Path = pathfinder.multiGoalFind(board.taverns, heroTile)

  override def run(): Path = {
    richHero map pathToRichHero getOrElse Path(Vector())
  }

  private def pathToRichHero(rh: Hero): Path = {
    val richHeroTile: PositionedTile = PositionedTile(Tile.Hero(rh.id), rh.pos)

    pathfinder.multiGoalFind(Vector(richHeroTile), heroTile)
  }

  override def validate(): ValidReason =
    richHero match {
      case Some(h) if !tavernNextToEnemy(h) => ValidReason(valid = true, s"Chasing richest hero: $h")
      case _ => ValidReason(valid = false)
    }

  private def tavernNextToEnemy(enemy: Hero) =
    enemy.pos.neighbors flatMap board.at exists(_.tile == Tavern)

  override def toString: String = "Chase Rich Hero"
}