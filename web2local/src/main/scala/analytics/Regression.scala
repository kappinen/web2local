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

import breeze.numerics._
import types.DataItem
import org.apache.commons.math.stat.regression.OLSMultipleLinearRegression
import org.apache.commons.math.stat.correlation.SpearmansCorrelation


object Regression {
  val SQRT2 = math.sqrt(2.0)

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

  def corr(x: Array[Double], y: Array[Double]): Double = correlation.correlation(x, y)

  def getModel(data: Seq[DataItem], opts: Map[String, String]): Map[String, Any] = ???


  def res2add(param: Array[Double], data: Array[Array[Double]]): Array[Array[Double]]
  = data.map((x) => param(0) +: (param drop 1, x).zipped.map(_ * _))

  def res2array(param: Array[Double], data: Array[Array[Double]]): Array[Double]
  = res2add(param, data).map((a) => a.reduce((a, b) => a + b))

  @Deprecated
  def mean(data: Array[Double]): Double = data.sum / data.size


  /* Standard Deviation */
  def stdDev(data: Array[Double], mean: Double): Double
  = scala.math.sqrt(data.map((a) => (a - mean) * (a - mean)).sum / data.size)

  def stdDev(data: Array[Double]): Double = {
    val dataMean = mean(data)
    scala.math.sqrt(data.map((a) => (a - dataMean) * (a - dataMean)).sum / data.size)
  }

  def stdDevUnbiased(data: Array[Double], mean: Double): Double
  = scala.math.sqrt(data.map((a) => (a - mean) * (a - mean)).sum / (data.size - 1))

  def gaussianStdDevSigma(p: Double, x: Double, mean: Double) = (x - mean) / (SQRT2 * erfi(2 * p - 1))

  def nstdScope(data: Array[Double]): Array[Double] = {
    val meanVal = mean(data)
    val stdVal = stdDev(data, meanVal)
    data.map((a) => (a - meanVal).toDouble / stdVal)
  }

  //  http://web2.uconn.edu/cyberinfra/module3/Downloads/Day%206%20-%20Hierarchical%20Bayes.pdf
  def simpleFit(stable: Array[Double]): Seq[Double] = {
    val mu = breeze.linalg.mean(stable)
    (1 to stable.length + 1).map(a => mu * a.toDouble)
  }

  /* Walter Chang - http://stackoverflow.com/questions/1319891/calculating-the-moving-average-of-a-list */
  def sma(vs: Array[Double], p: Int): Array[Double] =
    ((vs.take(p).sum / p :: List.fill(p - 1)(0.0), vs) /: vs.drop(p)) { (a, v) =>
      ((a._1.head - a._2.head / p + v / p) :: a._1, a._2.tail)
    }._1.reverse


  def diff(data: Seq[Double]): Seq[Double] = (data.drop(1) zip data.dropRight(1)).map((a) => a._1 - a._2)

  def diffLeft(data: Seq[Double]): Seq[Double] = (data.drop(1) zip data.dropRight(1)).map((a) => a._1 - a._2)

  def diffProcents(data: Seq[Double]): Seq[Double] = (data.drop(1) zip data.dropRight(1)).map((a) => a._2 / a._1)

  /**
   * * FIXME: fix :+ -> +:
   * @param data
   * @param x
   * @return
   */
  @Deprecated
  def emaOLd(data: Array[Double], x: Int): Seq[Double] = {

    if (data.length <= 1) {
      data.toSeq
    } else {
      val previousEma = ema(data.dropRight(1), x)
      previousEma :+ (previousEma.last + (2d / (x + 1d)) * (data.last - previousEma.last))
    }
  }

  //http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages
  def ema(data: Array[Double], N: Int): Seq[Double] = {
    val alfa: Double = 2d / (N + 1).toDouble
    var S_1: Double = data(0)

    data.map((x) => {
      val z = alfa * (x - S_1) + S_1
      S_1 = z
      z
    })
  }

  /** Relative strength index
    * http://en.wikipedia.org/wiki/Relative_strength_index
    */
  def rsi(movingAverage: (Array[Double], Int) => Array[Double] = ema)
         (data: Array[Double], N: Int) = {

    val diffVal = diff(data)
    val u = diffVal.map((a) => if (0.0d <= a) a else 0.0d)
    val d = diffVal.map((a) => if (0.0d > a) -1 * a else 0.0d)

    (movingAverage(u.toArray, N) zip movingAverage(d.toArray, N)).map((a) => 100d - (100d / (1 + a._1 / a._2)))
  }

  def sharpeRatio(returns: Array[Double], riskFreeReturn: Double): Double = {
    (mean(returns) - riskFreeReturn) / stdDev(returns, mean(returns))
  }

  //Compound Annual Growth Rate
  def cagr(endingValue: Double, beginningValue: Double, nyars: Int): Double = (math.pow(endingValue / beginningValue, (1 / nyars.toDouble)) - 1) * 100d


  //TODO: Commodity Channel Index (CCI)
  //TODO: http://www.investopedia.com/terms/h/heikinashi.asp

  def test() = {
    val testset = Array[Double](22.27, 22.19, 22.08, 22.17, 22.18, 22.13, 22.23, 22.43, 22.24, 22.29, 22.15, 22.39, 22.38, 22.61, 23.36, 24.05, 23.75, 23.83, 23.95, 23.63, 23.82, 23.87, 23.65, 23.19, 23.10, 23.33, 22.68, 23.10, 22.40, 22.17)
    sma(testset, 10)
    ema(testset, 10)
    cagr(19500, 10000, 3) == 24.933297746139083




  }
}
