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
   * @param comparator
   * @param setA
   * @param setB
   * @tparam T
   * @return a subset of SetB intersection of SetA
   */
  def intersectBy[T](comparator: (T, T) => Boolean)(setA: Seq[T], setB: Seq[T]): Seq[T] = {
    setA.map((a) => setB.find((b) => comparator(a, b))).filter((z) => z != None).map((a) => a.get.asInstanceOf[T])
  }

  /**
   * Merges lists by comparator.
   * @param comparator comparator
   * @param args lists
   * @tparam T type
   * @return merged list
   */
  def mergeBy[T](comparator: (T, T) => Boolean)(args: Seq[T]*): Seq[Seq[T]] = {

    val minSize = args.reduce((a, b) => if (a.size < b.size) a else b)
    val newArgs = args.map((ar) => intersectBy[T](comparator)(minSize, ar))
    val newMin = newArgs.reduce((a, b) => if (a.size < b.size) a else b)

    if (minSize.size == newMin.size)
      return newArgs
    else
      return mergeBy[T](comparator)(newArgs: _*)
  }

  /**
   *
   * @param func changes an original value for a key within a data item.
   * @param key data object with data item
   * @param dataSeq a sequence of data items
   * @return a new sequence of data items
   */
  def applyData(func: (Any) => Any)(key: String, dataSeq: Seq[DataItem]): Seq[DataItem] = {
    dataSeq.map((data) => new DataItem(data.source, data.dtime, data.tags, data.data.updated(key, func(data(key)))))
  }

  /**
   * Generalization for sliding window based indicators.
   * @param func function takes Seq as an argument and should return the last element with the changed value of T
   * @param dataSeq Initial data sequence
   * @param minSize Minimum size of Sliding window
   * @tparam T Data Item
   * @return changed sequence of data items
   */
  def applyRecursively[T](func: (Seq[T]) => T)
                         (dataSeq: Seq[T], minSize: Int): Seq[T] = {

    val mResult: Seq[T] = {
      if (dataSeq.size <= minSize) {
        dataSeq
      } else {
        applyRecursively(func)(dataSeq.dropRight(1), minSize)
      }
    }

    return mResult :+ func(mResult :+ dataSeq(dataSeq.size - 1))
  }

  /**
   *  A slide counter
   * @param dataSeq Initial data sequence
   * @param minSize Minimum size of Sliding window
   * @tparam T Data Item
   * @return changed sequence of data items
   */
  def slideCounter[T](dataSeq: Seq[T], minSize: Int): Map[String, Int] = {
    var map:Map[String, Int] = Map()

    val iter = dataSeq.sliding(minSize)
    while(iter.hasNext) {
      val sig = iter.next.mkString
      val counter = map.getOrElse(sig, 0)
      map += sig -> (counter + 1)
    }
    map
  }

}
