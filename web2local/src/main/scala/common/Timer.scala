package common

/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 5/11/13
 * Time: 5:58 PM
 * Source: https://gist.github.com/jamesthompson/5120519
 * http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
 */
object Timer {
  def time[R](block: => R): R = timer(block)("Elapsed time")

  def timer[R](block: => R)(msg:String = "Elapsed time"): R = {
    val start = System.nanoTime
    val result = block
    val end = System.nanoTime
    println(msg + ": (" + block.getClass.getName + ") " + (end - start) + "ns " + (end - start) / (1000 * 1000) + "ms " + (end - start) / (1000 * 1000 * 1000).toDouble + "s")
    result
  }

  private def exampleOfTimer() = {
    common.Timer.timer { println("Hi") }("spent")
    common.Timer.time { println("Hi") }
  }
}