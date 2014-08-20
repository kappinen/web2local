package stock

import common.Utils._
import types.DataItem
import analytics.Plots._

/**
 * Created with IntelliJ IDEA.
 * User: Evgeni Kappinen
 * Date: 8/15/13
 * Time: 6:51 PM
 
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

class MarketState {
  var budjet = 3000 * 100
  var stocks = 0
  var orders: Seq[DataItem] = Seq()

  var statHighest = budjet;
  var statLowest = budjet;
  var avgLost = 0;
  var avgProfit = 0;

  val commission = 900
  val initBudjet = budjet

  def buySellPrice(item: DataItem):Double = ((item.toDouble("High price") + item.toDouble("Low price")) / 2.toDouble + item.toDouble("Closing price")) / 2.toDouble

  def buy(price: DataItem) : MarketState = {
    if (stocks == 0 && budjet > 0) {
      budjet -= commission
      val dprice = buySellPrice(price) * 100
      stocks = (budjet / dprice).toInt
      budjet -= (stocks * dprice.toInt)
      orders = orders :+ DataItem(price.source, price.dtime, price.tags ++ List("Buy"), price.data)
      println("Buying:" + epoc2str(price.dtime) + " for:" + buySellPrice(price))
    }
    return this
  }


  def sell(price: DataItem): MarketState = {
    if (stocks != 0) {
      budjet -= commission
      val dprice = buySellPrice(price) * 100
      budjet += (stocks * dprice.toInt)
      stocks = 0
      if (budjet > statHighest)  statHighest = budjet
      if (budjet < statLowest)  statLowest = budjet
      orders = orders :+ DataItem(price.source, price.dtime, price.tags ++ List("Sell"), price.data)
      println("   ** Selling:" + epoc2str(price.dtime) + " for:" + buySellPrice(price) + " gain/lost:" + (dprice - getLastPrice()) / 100.toDouble)
    }
    return this
  }


  def getLastPrice() : Int = (buySellPrice(orders.last) * 100).toInt


  def report(): (Double, Int) = {
    val profit = ((getLastPrice() * stocks + budjet).toDouble - initBudjet) / 100.toDouble

    val profitQueue = new scala.collection.mutable.Queue[Double]
    val lostQueue = new scala.collection.mutable.Queue[Double]

    val buys = orders.filter((a) => a.tags.contains("Buy"))
    val sells = orders.filter((a) => a.tags.contains("Sell"))

    (buys zip sells).map((a) =>  {
      val pr = (buySellPrice(a._2) * 100).toInt - (buySellPrice(a._1) * 100).toInt;
      if (pr > 0.00d)
        profitQueue.enqueue(pr)
      else
        lostQueue.enqueue(pr)
    })

    println(">==" + profit + "=========================================================================")
    println("Budjet Highest:" + statHighest / 100 + " Lowest:" + statLowest / 100 + " Inital:" + initBudjet / 100)
    println("Avg. Profit:" +  profitQueue.sum / profitQueue.size + " Avg. Lost:" + lostQueue.sum / lostQueue.size)
    println("Orders Total:" + orders.size + " Profitable:" + profitQueue.size + " Lost:" + lostQueue.size)
    println("<-----------------------------------------------------------------------------------------")

    (profit, orders.size)
  }

  def dumpBuysSells() = {
    orders.map((a) => {
      println(a.tags.mkString(" :") + " date:" + a("Date").toString + " price:" + buySellPrice(a))
    })
  }


  def plotData(marketData:Seq[DataItem]) {

    val prices = marketData.map((a) => buySellPrice(a))
    val dates = marketData.map((a) => str2date(a("Date").toString).getMillis.toDouble)
    plot(dates, prices, '-')

    val buys = orders.filter((a) => a.tags.contains("Buy"))
    val sells = orders.filter((a) => a.tags.contains("Sell"))

    val vbuy = buys.map((a) => buySellPrice(a))
    val dbuy = buys.map((a) => str2date(a("Date").toString).getMillis.toDouble)
    plota(dbuy, vbuy, '+')

    val vsell = sells.map((a) => buySellPrice(a))
    val dsell = sells.map((a) => str2date(a("Date").toString).getMillis.toDouble)
    plota(dsell, vsell, '+')
  }

}