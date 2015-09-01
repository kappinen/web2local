package stock

import analytics.Regression._
import io.LocalStorage._
import types.{DataItemUtil, DataItem}
import stock.MarketStrategy._
import common.Utils._
import analytics.Plots._
import types.DataItemExtension._
import analytics.Plots._
import types.DataItemUtil._
import scala.math._
import types.DataItem
import stock.MarketState
import stock.MarketStrategy._


/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 8/15/13
 * Time: 6:54 PM

The MIT License (MIT)

Copyright (c) <2013> <Evgeni Kappinen>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

 */
object MarketSignals {


  def sellStopLost(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    if (market.orders.size != 0 && market.orders.last.tags.contains("Buy")) {
      if (buySellPrice(market.orders.last) * 100 * params("StopLost").toDouble >
        buySellPrice(data.last) * 100.toDouble)
        return true
    }
    return false
  }


  def sellStopWin(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    if (market.orders.size != 0 && market.orders.last.tags.contains("Buy")) {
      if (buySellPrice(market.orders.last) * 100 * params("StopWin").toDouble <
        buySellPrice(data.last) * 100.toDouble)
        return true
    }
    return false
  }

  def sellstopSMAWin(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    if (market.orders.size != 0 && market.orders.last.tags.contains("Buy")) {

      val marketData = data.applyTo("fixedPrice_")((a) => buySellPrice(a)).toDouble("fixedPrice_")
      val sfast = sma(marketData.takeRight(params("RsiMax").toInt), 4)
      val sslow = sma(marketData.takeRight(params("RsiMax").toInt), 20)
      if (sfast.last > sslow.last) {
        return true
      }
    }

    return false
  }


  def buyRsi(market: MarketState, data: Seq[DataItem], params:Map[String, String]): Boolean = {

    val marketData = data.applyTo("fixedPrice_")((a) => buySellPrice(a)).toDouble("fixedPrice_")

    val sfast = sma(marketData.takeRight(params("RsiMax").toInt), params("SMAFastN").toInt)
    val sslow = sma(marketData.takeRight(params("RsiMax").toInt), params("SMASlowN").toInt)

    val rsiVal = rsi(ema)(marketData.takeRight(params("RsiMax").toInt).toArray, params("RsiPeriod").toInt)
    if (rsiVal.last > params("RsiBuyLimit").toDouble &&
          sfast.last < sslow.last) {
        return true
    }

    return false
  }

  def sellRsi(market: MarketState, data: Seq[DataItem], params:Map[String, String]): Boolean = {

    val marketData = data.applyTo("fixedPrice_")((a) => buySellPrice(a)).toDouble("fixedPrice_")

    val sfast = sma(marketData.takeRight(params("RsiMax").toInt), params("SMAFastN").toInt)
    val sslow = sma(marketData.takeRight(params("RsiMax").toInt), params("SMASlowN").toInt)

    val rsiVal = rsi(ema)(marketData.takeRight(params("RsiMax").toInt).toArray, params("RsiPeriod").toInt)
    if (rsiVal.last > params("RsiSellLimit").toDouble) {
      if (sfast.last > sslow.last) {
        return true
      }
    }
    return false
  }


  def randomWalk(market: MarketState, data: Seq[DataItem], params:Map[String, String]): Boolean = (math.random *10).toInt % 2 == 1
  def randomWalk2(market: MarketState, data: Seq[DataItem], params:Map[String, String]): Boolean = (math.random *100).toInt % 20 == 1


  def vpt2(dataSeq: Seq[DataItem]): DataItem = {
    val window = dataSeq.takeRight(2)
    val today = window(1)
    val previous = window(0)
    val prevVPT:Int = if (previous.data.contains("Count")) {
      previous("Count").toString.toInt
    } else { 0 }


    new DataItem(today.source, today.dtime, today.tags, today.data.updated("Count", prevVPT + 1))
  }

  def vpt(dataSeq: Seq[DataItem]): DataItem = {

    val window = dataSeq.takeRight(2)
    val today = window(1)
    val previous = window(0)

    val prevVPT = if (previous.data.contains("VPT")) {
      previous("VPT").toString.toDouble
    } else { 0.0d }


    val closeDiff = (Option(today("Total volume")).getOrElse("1")).toDouble *
    (str2cents(today("Closing price")) - str2cents(previous("Closing price"))) /
      str2cents(previous("Closing price")).toDouble

    val VPT = prevVPT + closeDiff

    new DataItem(today.source, today.dtime, today.tags, today.data.updated("VPT", VPT))
  }

}
