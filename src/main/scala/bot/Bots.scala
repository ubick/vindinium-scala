package bot

import bot.Dir._

trait Bot {
  def move(input: Input): Dir
}

class BasicBot extends Bot {
  override def move(input: Input): Dir = {
    val startTime: Long = System.nanoTime
    val dir = GameEngine.play(input)
    val durationMilliseconds: Double = (System.nanoTime - startTime) * 1000 / 1e9d
    println(s"Took $durationMilliseconds ms to compute move")

    dir
  }
}

