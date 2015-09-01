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


  def hist(x: Seq[Double]) = {
    figure.clear()
    val p = figure.subplot(0)
    p += breeze.plot.hist(x)
  }

  def plotx(data: Seq[Double]*) = {
    data.map((chart) => plota((1 to chart.size).map((a) => a.asInstanceOf[Double]), chart, '-'))
  }


  def plotWithDate(data: Seq[DataItem], dataKey: String) =
    plota(data.map((a) => (str2date(a("Date").toString).getMillis.toDouble / (24 * 60 * 60 * 1000)).toInt.toDouble), data.toDouble(dataKey), '-')


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

    def draw(): Unit = plotN.++=(series)

  }

}
