package types

import common.XConstants

import scala.collection.{mutable, Traversable}
import types.DataExtension._
/**
 * Created by Evgeni Kappinen on 5/24/14.
 */
object DataItemExtension {

  implicit class DataItemHelper(sequence: Seq[DataItem]) {

    def apply(key: String): Seq[Any] = sequence.map((a) => a(key))

    def applyTo(newKey: String)(func: (DataItem) => Any): Seq[DataItem] = sequence.map((a) => a ++ Map(newKey -> func(a)))

    def slidingBy(newKey: String, window: Int)(func: (Seq[DataItem]) => Any): Seq[DataItem] = sequence.sliding(window).toSeq.map((a) => a.last ++ Map(newKey -> func(a)))


    def minMaxBy(key: String): Pair[Double, Double] =
      sequence.foldLeft[Pair[Double, Double]](Pair(1, 0))((a, b) => (math.min(b(key).toDouble, a._1), math.max(b(key).toDouble, a._2)))

    //TODO: , size: Int = 2 and slideBy
    def meanBy(key: String): Double = sequence.toDouble(key).reduce((a, b) => b + a) / sequence.size

    /**
     *
     * @param key
     * @return if day_1 = 12, day_2 = 11 then 11-12 as day_2
     */
    def diffBy(key: String, newKey: String = null): Seq[DataItem] =
      (sequence.drop(1) zip sequence.dropRight(1)).
        map((a) => a._1 ++ Map({if (newKey == null) key else newKey} -> (a._1.toDouble(key) - a._2.toDouble(key))))

    def normilize(key: String): Seq[DataItem] = {
      val limits = minMaxBy(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - limits._1) / (limits._2 - limits._1)))
    }

    def zscore(key: String): Seq[DataItem] = {
      val limits = minMaxBy(key)
      val mean = meanBy(key)

      sequence.map((a) => a ++ Map(key -> (a(key).toDouble - mean) / (limits._2 - limits._1)))
    }

    def toDouble(key: String): Seq[Double] = sequence.map((a) => a.toDouble(key))

    def getOrDefault[T](key: String, defaultValue: T): Seq[T] = {
      sequence.map((a) => {
        if (a(key) == null) {
          defaultValue
        } else {
          a(key).asInstanceOf[T]
        }
      })
    }

    def sma(x: Integer, name: String = null, sname: String = XConstants.NORM_PRICE) =
      sequence.slidingBy( if (name == null) { "SMA" + x } else { name } , x)(a => a.toDouble(sname).sum / a.size.toDouble)

    def vsma(x: Integer, name: String = null, sname: String = XConstants.NORM_PRICE, volumeName:String = XConstants.VOLUME) =
      sequence.slidingBy( if (name == null) { "VSMA" + x } else { name } , x)(a => (a.toDouble(XConstants.NORM_PRICE) * a.toDouble(volumeName)).sum / a.toDouble(volumeName).sum.toDouble)

    def rsi(x: Integer, name: String = null, rname: String = XConstants.NORM_PRICE)(movingAverage: (Array[Double], Int) => Array[Double] = analytics.Regression.sma) = {
      val rsiArray = analytics.Regression.rsi(movingAverage)(sequence.toDouble(rname).toArray, x)
      (sequence.drop(1) zip rsiArray).map(a => a._1 ++ Map({if (name == null) { "RSI" + x } else { name }} -> a._2))
    }

    def volatility() : Double = {
      sequence.getNormPrice().slidingBy("Returns price", 2)(cc => {
        val norm = cc.toDouble(XConstants.NORM_PRICE)
        if ((norm(0) / norm(1).toDouble) > 1d) {
          (norm(0) / norm(1).toDouble) - 1d
        } else {
          1d - (norm(0) / norm(1).toDouble)
        }
      }).toDouble("Returns price").unbiasstdev()
    }

    def getNormPrice(normPriceName: String = XConstants.NORM_PRICE, a: String = "High price", b: String = "Low price"): Seq[DataItem] = {
      sequence.slidingBy(normPriceName, 1)((item) => {
        (item.toDouble(a).sum + item.toDouble(b).sum) / 2d
      })
    }
  }

  implicit class DataItemHelperSeq(sequence: Traversable[Seq[DataItem]]) {

    def apply(key: String): Seq[DataItem] = getBySource(key)

    def getBySource(name: String): Seq[DataItem] =
      sequence.find((a) => name.equals(a(0).source)).getOrElse(Seq())


    def pairRun[T](func: (Seq[DataItem], Seq[DataItem]) => T, mergeName: String = "Date", printProgress: Boolean = false): Seq[(String, T)] = {
      val map = mutable.ListMap[String, T]()

      val totalCount = (sequence.size * sequence.size)
      val start = System.currentTimeMillis()
      var count = 0
      sequence.foreach(aa => {
        sequence.foreach(bb => {

          count = count + 1
          if (printProgress && (count % 100 == 0)) {
            val progressed = (System.currentTimeMillis() - start) / count.toDouble
            println("Calculated:" + count + " out of:" + totalCount +
              " left:" + (common.Utils.millisDiffToTime((totalCount - count) * progressed)))
          }

          if (map.contains(aa(0).source + "," + bb(0).source) ||
            map.contains(bb(0).source + "," + aa(0).source) ||
            bb(0).source.equals(aa(0).source)) {
          } else {
            val merged = DataItemUtil.mergeBy((a: DataItem, b: DataItem) => a(mergeName).equals(b(mergeName)))(aa, bb)
            map.put(aa(0).source + "," + bb(0).source, func(merged(0), merged(1)))
          }

        })
      })

      map.toSeq
    }

  }


  //http://rosettacode.org/wiki/Averages/Median
  def median(sequence: Array[Double]): Double = {
    val sorted = sequence.sorted
    val splitted = sorted.splitAt(sorted.size / 2)
    if (sorted.size % 2 == 0) {
      return (splitted._1.last + splitted._2.head) / 2d
    } else {
      splitted._2.head
    }
  }

}

