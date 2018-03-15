package bot.state

import bot._
import bot.pathfind.{Path, Pathfinder}

trait Behavior {
  def run(): Path
  def validate(): ValidReason
}




//val myHeroFilter: Hero => Boolean = (hero: Hero) => List("Ubick", "Liviu", "Oana").exists(hero.name ==)
//val startTime: Long = System.nanoTime
//val minLifeBeforeTavern = 41
//val hero: Hero = input.hero
//val board: Board = input.bot.game.board
//val positionedBoard: PositionedBoard = PositionedBoard(board.size, PositionedBoard.positionedTiles(board))
//val taverns: List[Pos] = positionedBoard.taverns
//val enemyHeroes: List[Hero] = input.bot.game.heroes filterNot myHeroFilter
//val mines: List[Pos] = positionedBoard.otherMinesPositions(enemyHeroes)
//val enemyHeroesPositions: List[Pos] = input.bot.game.heroes filterNot myHeroFilter map { _.pos }



//        val mineObjective: Option[ScoredPos] = closestObjective(mines)
//        val tavernObjective: Option[ScoredPos] = closestObjective(taverns)
//        val heroObjective: Option[ScoredPos] = closestObjective(enemyHeroesPositions)
//        val closestHeroRef: Hero = input.bot.game.heroes filter {_.pos == heroObjective.get.objectivePos.pos} head
//
//        if (enemyHeroesPositions.isEmpty) {
//          Stay
//        }// this might crash, but we should always have taverns
//        // If tavern is next to us and life is below 100, then move towards it
//        else if (tavernObjective.get.pathLength == 1 && hero.life < 95) {
//          moveTowardsClosestObjective(tavernObjective)
//        } else {
//          mineObjective match {
//              // Head towards a tavern if there's no mine left to conquer
//              case None => moveTowardsClosestObjective(tavernObjective)
//              case Some(sp) => {
//                // only attack heroes that are closer to us than the nearest mine and own at least 1 mine
//                if (heroObjective.get.pathLength <= 4 &&
//                  heroObjective.get.pathLength <= sp.pathLength &&
//                  positionedBoard.positionedTiles.exists(_.tile == Mine(Some(closestHeroRef.id))) &&
//                  closestHeroRef.pos != closestHeroRef.spawnPos
//                ) {
//                  if (hero.life > closestHeroRef.life + 21) {
//                    // Attack weak hero close to current position
//                    moveTowardsClosestObjective(heroObjective)
//                  } else {
//                    // Run away from strong hero close to current position
//
//                    val neighbors = hero.pos.neighbors filter {nb => heroObjective.get.nextPos.exists(_ != nb)}
//                    val escapeRoutes: Set[Pos] = neighbors.filter { p => positionedBoard.at(p).exists(t => t.tile == Air || t.tile == Tavern) }
//
//                    nextDir(escapeRoutes.headOption)
//                  }
//
//                } else {
//                  val requiredLifeToReachMine = minLifeBeforeTavern + sp.pathLength
//
//                  println(s"HP: ${hero.life} Life req for mine: $requiredLifeToReachMine")
//
//                  if (hero.life < requiredLifeToReachMine) {
//                    // Move towards tavern if life is insufficient to travel and conquer nearest tavern
//                    moveTowardsClosestObjective(tavernObjective)
//                  } else {
//                    // Move towards mine if life is sufficient
//                    moveTowardsClosestObjective(mineObjective)
//                  }
//              }
//            }
//          }
//        }
//      }