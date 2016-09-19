/*
 * The MIT License (MIT)
 *
 * Copyright (c) <2013> <Evgeni Kappinen>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package analytics

import analytics.Regression._
import breeze.plot._
import common.Utils._
import types.DataItem
import types.DataItemExtension._

import scala.collection.mutable.ListBuffer

object Plots {
  var figure = Figure()
  var plotN = figure.subplot(0)

  /* Simply creates a new plot*/
  def plot(x: Seq[Double], y: Seq[Double], style: Char = '.'): Plot = {
    figure.clear()
    plotN = figure.subplot(0)
    plotN += breeze.plot.plot(x, y, style)

  }

  def plotRefresh() = {
    figure.clear()
    figure.rows = 1
    figure.cols = 1
    plotN = figure.subplot(0)
    figure.visible = true
    this
  }

  def plotNew() = {
    figure = Figure()
    plotN = figure.subplot(0)
    this
  }

  def plotNewColumn() = {
    figure.rows = figure.rows + 1
    plotN = figure.subplot(figure.rows - 1)
    this
  }

  def plota(x: Seq[Double], y: Seq[Double],
            style: Char = '.', colorcode: String = null,
            name: String = null, lines: Boolean = true,
            shapes: Boolean = false, labels: (Int) => String = null.asInstanceOf[Int => String],
            tips: (Int) => String = null.asInstanceOf[Int => String]) = {
    figure.visible = true
    plotN += breeze.plot.plot(x, y, style, colorcode, name, lines, shapes, labels, tips)
  }


  def plotc(x: Seq[Double], y: Seq[Double], style: Char = '.') = {
    figure.clear()
    plotN = figure.subplot(0)

    plotN += breeze.plot.plot(x, y, style)
  }

  //https://en.wikipedia.org/wiki/Percentile
  //https://github.com/haifengl/smile/blob/24e48593b703d747f80905ca8f290b7d2994c514/plot/src/main/java/smile/plot/QQPlot.java
  def ordinalRank(percentile: Integer, size: Integer): Double = {
    (percentile / 100d) * size.toDouble
  }

  def qqpplot(x: Seq[Double], y: Seq[Double]) = {
    val xsorted = x.sorted
    val ysorted = y.sorted

    val size = Math.min(x.length, y.length)

    val pquntiles = (0 to 99).map(percentile => {
      (xsorted(Math.round((percentile / 100d) * x.length.toDouble).toInt),
        ysorted(Math.round((percentile / 100d) * y.length.toDouble).toInt))
    })

    plot(pquntiles.map(a => a._1), pquntiles.map(a => a._2))
  }

  def qqplot(x: Seq[Double], y: Seq[Double]) = {
    val xsorted = x.sorted
    val ysorted = y.sorted

    val size = Math.min(x.length, y.length)

    val quntiles = (0 to size - 1).map((a) => {
      val p = (a + 1) / (size + 1.0d)
      (xsorted(Math.round(p * x.length.toDouble).toInt),
        ysorted(Math.round(p * y.length.toDouble).toInt))
    })

    plot(quntiles.map(a => a._1), quntiles.map(a => a._2))
    val array = Regression.regress(quntiles.map(a => a._1).toArray, quntiles.map(a => a._2).toArray)
    plotx((1 to size).map(x=> array(1)*x + array(0)))
  }

  def hist(x: Seq[Double], bins : HistogramBins = 20, name : String = null) = {
    figure.clear()
    val p = figure.subplot(0)
    p += breeze.plot.hist(x, bins, name)
  }

  def plotx(data: Seq[Double]*) = {
    data.map((chart) => plota((1 to chart.size).map((a) => a.asInstanceOf[Double]), chart, '-'))
  }

  def plotxLine(data: Seq[Double]*) = {
    data.map((chart) => plota((1 to chart.size).map((a) => a.asInstanceOf[Double]), simpleFit(diffLeft(chart)), '-'))
  }


  def plotWithDate(data: Seq[DataItem], dataKey: String) =
    plota(data.map((a) => a.toMillis("Date")), data.toDouble(dataKey), '-')


  implicit class PlotsHelper(sequence: Seq[DataItem]) {
    def plotBy(key: String): Seq[DataItem] = {
      plotx(sequence.toDouble(key));
      sequence
    }
  }


  class PlotBatch {
    val series = new ListBuffer[Series]()

    def plota(x: Seq[Double], y: Seq[Double],
              style: Char = '.', colorcode: String = null,
              name: String = null, lines: Boolean = true,
              shapes: Boolean = false, labels: (Int) => String = null.asInstanceOf[Int => String],
              tips: (Int) => String = null.asInstanceOf[Int => String]) = {
      series += breeze.plot.plot(x, y, style, colorcode, name, lines, shapes, labels, tips)
    }

    def draw(): Unit = {
//      plotN.legend = true
      plotN.++=(series)
    }

  }



  implicit class PlotsExtensionHelper(plothelper: Plot) {

    def clean() = {
      figure.clear()
      figure.rows = 1
      figure.cols = 1
      plotN = figure.subplot(0)
      figure.visible = true
      this
    }
  }
}
