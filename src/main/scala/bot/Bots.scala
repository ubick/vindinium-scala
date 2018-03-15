package bot

import bot.Dir._

trait Bot {
  def move(input: Input): Dir
}

class BasicBot extends Bot {
  override def move(input: Input): Dir = {
    val startTime: Long = System.nanoTime
    val output = GameEngine.play(input)
    val ms: Double = (System.nanoTime - startTime) * 1000 / 1e9d

    println(s"[$ms ms] ${output.reason} ")

    output.dir
  }
}

