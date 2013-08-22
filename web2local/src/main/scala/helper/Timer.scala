package helper

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

  def timer[R](block: => R)(msg:String): R = {
    val start = System.nanoTime
    val result = block
    val end = System.nanoTime
    println(msg + ": ("+ block.getClass.getName +") " + (end - start) + "ns")
    result
  }
}