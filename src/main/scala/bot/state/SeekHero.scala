package bot.state

import bot.GameEngine.GameContext
import bot.Tile.Tavern
import bot._
import bot.pathfind.{Path, Pathfinder}

class SeekHero(implicit val context: GameContext) extends Behavior {
  val hero: Hero = context.hero
  val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
  val board: PositionedBoard = context.board
  val pathfinder: Pathfinder = context.pathfinder
  val enemyHeroes: Vector[Hero] = context.enemyHeroes filter isEnemyAttackable filterNot tavernNextToEnemy
  val enemyHeroesTiles: Vector[PositionedTile] = enemyHeroes map { h => PositionedTile(Tile.Hero(h.id), h.pos) }
  val closestTavernPath: Path = pathfinder.multiGoalFind(board.taverns, heroTile)
  val path: Path = pathfinder.multiGoalFind(enemyHeroesTiles, heroTile)

  override def run(): Path = path

  override def validate(): ValidReason =
    if (closestTavernPath.length <= 2) ValidReason(valid = false)
    else if (path.isNonEmpty && path.length < 4) ValidReason(valid = true, s"Hero is ${path.length} tiles away.")
    else ValidReason(valid = false)

  private def isEnemyAttackable(enemy: Hero): Boolean =
    hero.life - enemy.life > 20 &&
    enemy.mineCount > 0 &&
    enemy.pos != enemy.spawnPos

  private def tavernNextToEnemy(enemy: Hero) =
    enemy.pos.neighbors flatMap board.at exists(_.tile == Tavern)

  override def toString: String = "Seek Hero"
}