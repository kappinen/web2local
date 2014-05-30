package analytics
/*
http://en.wikipedia.org/wiki/Categorical_variable

> http://data.princeton.edu/wws509/notes/

>> http://stats.stackexchange.com/questions/11645/coding-an-interaction-between-a-nominal-and-a-continuous-predictor-for-logistic
>>> Relations can be described as
>>> x+y ==>[x,y] => x + y
>>> x*y ==> [x,y,x*y] => x + y + x*y with interception
>>>> http://stats.stackexchange.com/questions/48854/why-am-i-getting-different-intercept-values-in-r-and-java-for-simple-linear-regr


 y[1] ~ [x1,x2] => relation: y~x1+x2
 y[1] ~ x[x1,x2,x3] => y~x1+x2+x1*x2 => (relation:y~x1*x2)
 */
import types.DataItem
import org.apache.commons.math.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math.stat.correlation.SpearmansCorrelation


object Regression {
  implicit def array2data(x: Array[Double]): Array[Array[Double]] = x.map((z) => Array(z))
  implicit def seq2array(x: Seq[Double]): Array[Double] = x.toArray[Double]

  val regression = new OLSMultipleLinearRegression()
  val correlation = new SpearmansCorrelation()

  def regress(y: Array[Double], x: Array[Array[Double]]): Array[Double] = {
    regression.newSampleData(y, x)
    regression.estimateRegressionParameters()
  }

  def multiply(x: Array[Double], y: Array[Double]): Array[Array[Double]]
            = (y zip x).map((a) => Array(a._1, a._2, a._1 * a._2))

  def corr(x: Array[Double], y: Array[Double]): Double = correlation.correlation(x,y)

  def getModel(data:Seq[DataItem], opts:Map[String, String]) : Map[String, Any] = ???


  def res2add(param:Array[Double], data:Array[Array[Double]]) : Array[Array[Double]]
        = data.map((x)=> param(0) +: (param drop 1, x).zipped.map(_ * _))

  def res2array(param:Array[Double], data:Array[Array[Double]]) : Array[Double]
        = res2add(param, data).map((a) => a.reduce((a,b) => a + b))

  @Deprecated
  def mean(data: Array[Double]): Double = data.reduce((a, b) => b + a).toDouble / data.size


  /* Standard Deviation */
  def stdDev(data: Array[Double], mean:Double): Double
        = scala.math.sqrt(data.map((a) => (a - mean)*(a - mean)).reduce((a,b) => b + a))


  def nstdScope(data: Array[Double]): Array[Double] = {
    val meanVal = mean(data)
    val stdVal = stdDev(data, meanVal)
    data.map((a) => (a - meanVal).toDouble / stdVal)
  }


  /* Walter Chang - http://stackoverflow.com/questions/1319891/calculating-the-moving-average-of-a-list */
  def sma(vs: Array[Double], p: Int): Array[Double] =
    ((vs.take(p).sum / p :: List.fill(p - 1)(0.0), vs) /: vs.drop(p)) {(a, v) =>
      ((a._1.head - a._2.head / p + v / p) :: a._1, a._2.tail)
    }._1.reverse


  @Deprecated
  def diff(data: Seq[Double]): Seq[Double] =  (data.drop(1) zip data.dropRight(1)).map((a) => a._2 - a._1)


  /**
   * * FIXME: fix :+ -> +:
   * @param data
   * @param x
   * @return
   */
  def ema(data: Array[Double], x: Int): Seq[Double] = {

    if (data.length <= 1) {
      data.toSeq
    } else {
      val previousEma = ema(data.dropRight(1), x)
      previousEma :+ (previousEma.last + (2.toDouble / (x + 1)) * (data.last - previousEma.last))
    }
  }

  /** Relative strength index
   * http://en.wikipedia.org/wiki/Relative_strength_index
   *
   * @param movingAverage
   * @param data
   * @param N
   * @return
   */
  def rsi(movingAverage: (Array[Double], Int) => Array[Double] = ema)
         (data: Array[Double], N: Int) = {

    val diffVal = diff(data)
    val u = diffVal.map((a) => if (a < 0) 0.0 else a)
    val d = diffVal.map((a) => if (a > 0) 0.0 else -1 * a)

    (movingAverage(u.toArray, N) zip movingAverage(d.toArray, N)).map((a) => 100 - 100.toDouble / (1 + a._1 / a._2))
  }

}
