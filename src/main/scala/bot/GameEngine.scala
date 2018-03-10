package bot

import bot.Dir.Dir
import bot.pathfind.Pathfinder

object GameEngine {
  def play(input: Input): Dir = {
    val hero = input.hero
    val enemyHeroesTiles: Vector[PositionedTile] = input.game.heroes.toVector collect {
      case h if h.id != hero.id => PositionedTile(Tile.Hero(h.id), h.pos)
    }
    val heroTile = PositionedTile(Tile.Hero(hero.id), hero.pos)
    val pathFinder = new Pathfinder(input)
    val path = pathFinder.multiGoalFind(enemyHeroesTiles, heroTile)

    println(s"Moving ${path.nextDir} via ${path.next} towards ${path.destination}")

    path.nextDir
  }
}
