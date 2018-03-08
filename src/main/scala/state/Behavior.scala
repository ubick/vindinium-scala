package state

import bot.Dir.Dir
import bot._

trait Behavior {
  val priorities = List(
    "seek-hero",
    "seek-mine",
    "seek-tavern",
    "idle"
  )

  def run(input: Input): Dir
}

class SeekHero extends Behavior {
  override def run(input: Input): Dir = {
    val myHeroFilter: Hero => Boolean = (hero: Hero) => List("Ubick", "Liviu", "Oana").exists(hero.name ==)
    val startTime: Long = System.nanoTime
    val minLifeBeforeTavern = 41
    val hero: Hero = input.hero
    val board: Board = input.game.board
    val positionedBoard: PositionedBoard = PositionedBoard(board.size, PositionedBoard.positionedTiles(board))
    val enemyHeroesTiles: List[PositionedTile] = positionedBoard.positionedTiles filter { pt => input.game.heroes.filterNot(myHeroFilter).map {_.pos} contains pt.pos }
    val taverns: List[Pos] = positionedBoard.taverns
    val enemyHeroes: List[Hero] = input.game.heroes filterNot myHeroFilter
    val mines: List[Pos] = positionedBoard.otherMinesPositions(enemyHeroes)
    val enemyHeroesPositions: List[Pos] = input.game.heroes filterNot myHeroFilter map { _.pos }
//    val mineObjective: Option[ScoredPos] = closestObjective(mines)
//    val tavernObjective: Option[ScoredPos] = closestObjective(taverns)
//    val heroObjective: Option[ScoredPos] = closestObjective(enemyHeroesPositions)
//    val closestHeroRef: Hero = input.game.heroes filter {_.pos == heroObjective.get.objectivePos.pos} head

    Dir.Stay
  }
}

class Idle extends Behavior {
  override def run(input: Input): Dir = Dir.Stay
}