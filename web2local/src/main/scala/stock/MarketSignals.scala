package stock

import analytics.Regression._
import io.LocalStorage._
import types.DataItem
import stock.MarketStrategy._
import stock.MarketState
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
class MarketSignals {


  def sellStopLost(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    if (market.orders.size != 0 && market.orders.last.tags.contains("Buy")) {
      if (market.orders.last.data(market.buySellPrice).toString.replaceAll(",", ".").toDouble * 100 * params("StopLost").toDouble >
        data.last.data(market.buySellPrice).toString.trim.replaceAll(",", ".").toDouble * 100)
        return true
    }
    return false
  }


  def sellStopWin(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    if (market.orders.size != 0 && market.orders.last.tags.contains("Buy")) {
      if (market.orders.last.data(market.buySellPrice).toString.replaceAll(",", ".").toDouble * 100 * params("StopWin").toDouble <
        data.last.data(market.buySellPrice).toString.trim.replaceAll(",", ".").toDouble * 100)
        return true
    }
    return false
  }

  def sellstopSMAWin(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    if (market.orders.size != 0 && market.orders.last.tags.contains("Buy")) {

      val marketData = data_as[Double](market.buySellPrice, data)(market.buySellPrice)
      val sfast = sma(marketData.takeRight(params("RsiMax").toInt), 4)
      val sslow = sma(marketData.takeRight(params("RsiMax").toInt), 20)
      if (sfast.last > sslow.last) {
        return true
      }
    }

    return false
  }


  def buyRsi(market: MarketState, data: Seq[DataItem], params:Map[String, String]): Boolean = {

    val marketData = data_as[Double](market.buySellPrice, data)(market.buySellPrice)

    val sfast = sma(marketData.takeRight(params("RsiMax").toInt), 4)
    val sslow = sma(marketData.takeRight(params("RsiMax").toInt), 20)

    val rsiVal = rsi(ema)(marketData.takeRight(params("RsiMax").toInt).toArray, params("RsiPeriod").toInt)
    if (rsiVal.last > params("RsiLimit").toDouble) {
      if (sfast.last < sslow.last) {
        return true
      }
    }
    return false
  }


  def example_stock() = {

    val metso = csv(defParser)("resources/stock/MEO1V_FI0009007835-1990-01-01-2013-08-10.csv")

    val params = Map("RsiPeriod" -> "15",   //rsi(data,RsiPeriod)
                     "RsiLimit" -> "86.00", //when rsi singal is generated
                     "StopWin" -> "1.05",   //when sell signal is generated
                     "StopLost" -> "0.97",  //When, sell signal is generated
                     "RsiMax" -> "200")     //Max, of take from the past to test

    val second = testStrategy(Seq(sellStopLost, sellstopSMAWin), Seq(buyRsi))(metso.reverse.take(80), 20, new MarketState, params)
    second.raport()
     second.orders
   second.plotData(metso.take(80).reverse)
  }
}
