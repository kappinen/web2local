package types

import com.codahale.jerkson.Json._



object DataItemUtil {

  def obj2str[T](data: T): String = {
    generate[T](data)
  }

  def str2obj(objectdata: String): DataItem = {
    inClassLoader(classOf[DataItem])({
      parse[DataItem](objectdata)
    })
  }


  //Reference:   https://github.com/codahale/jerkson/issues/38
  private def inClassLoader[T](cls: Class[_])(f: => T): T = {
    val prev = Thread.currentThread.getContextClassLoader
    try {
      Thread.currentThread.setContextClassLoader(cls.getClassLoader)

      f
    } finally {
      Thread.currentThread.setContextClassLoader(prev)
    }
  }

  /**
   * Creates a subset of SetB intersection of SetA.
   * @param comparitor
   * @param setA
   * @param setB
   * @tparam T
   * @return a subset of SetB intersection of SetA
   */
  def intersectBy[T](comparitor: (T, T) => Boolean)(setA: Seq[T], setB: Seq[T]): Seq[T] = {
    setA.map((a) => setB.find((b) => comparitor(a, b))).filter((z) => z != None).map((a) => a.get.asInstanceOf[T])
  }

  /**
   * Merges lists by comparitor
   * @param comparitor comparitor
   * @param args lists
   * @tparam T type
   * @return merged list
   */
  def mergeBy[T](comparitor: (T, T) => Boolean)(args: Seq[T]*): Seq[Seq[T]] = {

    val minSize = args.reduce((a, b) => if (a.size < b.size) a else b)
    val newArgs = args.map((ar) => intersectBy[T](comparitor)(minSize, ar))
    val newMin = newArgs.reduce((a, b) => if (a.size < b.size) a else b)

    if (minSize.size == newMin.size)
      return newArgs
    else
      return mergeBy[T](comparitor)(newArgs: _*)
  }

}
