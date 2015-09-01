package stock

import common.Utils._
import io.LocalStorage._
import types.{DataItemUtil, DataItem}
import analytics.Plots._
import io.LocalStorage
import types.DataItemExtension._
import stock.MarketStrategy._


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

class MarketState(parameters: Map[String, String], printDebug: Boolean = false) {
  var budjet = 3000 * 100
  var stocks = 0

  var orders: Seq[DataItem] = Seq()

  var statHighest = budjet;
  var statLowest = budjet;

  var avgLost = 0;
  var avgProfit = 0;

  var biggestWin = 0;
  var biggestLost = 0;

  val commisionMin = 900
  val initialBudjet = budjet
  val params = parameters


  def commisionFunc(budjetValue: Int): Int = {
    if ((budjetValue * (0.20d / 100d)) > commisionMin) {
      (budjetValue * (0.20d / 100d)).toInt
    } else {
      commisionMin
    }
  }

  def buy(price: DataItem): MarketState = {
    if (stocks == 0 && budjet > 0) {
      budjet -= commisionFunc(budjet)
      val dprice = buySellPrice(price) * 100
      stocks = (budjet / dprice).toInt
      budjet -= (stocks * dprice.toInt)
      orders = orders :+ DataItem(price.source, price.dtime, price.tags ++ List("Buy"), price.data)
      if (printDebug) {
        println("Buying: " + epoc2str(price.dtime) + " for: " + buySellPrice(price))
      }
    }
    return this
  }


  def sell(price: DataItem): MarketState = {
    if (stocks != 0) {
      val sellingPrice = (buySellPrice(price) * 100).toInt
      budjet += (stocks * sellingPrice)
      budjet -= commisionFunc((stocks * sellingPrice))

      if (budjet > statHighest) statHighest = budjet
      if (budjet < statLowest) statLowest = budjet

      if ((sellingPrice * stocks - getLastPrice() * stocks) < biggestLost) {
        biggestLost = (sellingPrice * stocks - getLastPrice() * stocks)
      }
      if ((sellingPrice * stocks - getLastPrice() * stocks) > biggestWin) {
        biggestWin = (sellingPrice * stocks - getLastPrice() * stocks)
      }

      if (printDebug) {
        println("   ** Selling: " + epoc2str(price.dtime) + " for: " + buySellPrice(price) + " gain/lost: " + (sellingPrice * stocks - getLastPrice() * stocks) / 100d)
      }


      stocks = 0
      orders = orders :+ DataItem(price.source, price.dtime, price.tags ++ List("Sell"), price.data)
    }
    return this
  }

  def getLastPrice(): Int = (buySellPrice(orders.last) * 100d).toInt


  def report(): (Double, Int) = {
    if (orders.isEmpty) return (0, 0)

    val profit = ((getLastPrice() * stocks + budjet).toDouble) / 100.toDouble

    val profitQueue = new scala.collection.mutable.Queue[Double]
    val lostQueue = new scala.collection.mutable.Queue[Double]

    val buys = orders.filter((a) => a.tags.contains("Buy"))
    val sells = orders.filter((a) => a.tags.contains("Sell"))

    (buys zip sells).map((a) => {
      val pr = (buySellPrice(a._2) * 100).toInt - (buySellPrice(a._1) * 100).toInt;
      if (pr > 0.00d)
        profitQueue.enqueue(pr)
      else
        lostQueue.enqueue(pr)
    })

    println(">==[ " + profit + " ] =======================================================================")
    println("Budjet Highest:" + statHighest / 100 + " Lowest:" + statLowest / 100 + " Inital:" + initialBudjet / 100)
    println("Biggest win:" + biggestWin + " Biggest Lost:" + biggestLost)
    println("Avg. Profit:" + profitQueue.sum / profitQueue.size + " Avg. Lost:" + lostQueue.sum / lostQueue.size)
    println("Orders Total:" + orders.size + " Wins:" + profitQueue.size + " Losts:" + lostQueue.size)
    println("<-----------------------------------------------------------------------------------------")

    (profit, orders.size)
  }

  def dumpBuysSells() = {
    orders.foreach((a) => {
      println(a.tags.mkString(" :") + " date:" + a("Date").toString + " price:" + buySellPrice(a))
    })
  }


  def plotData(marketData: Seq[DataItem]) {

    val prices = marketData.map((a) => buySellPrice(a))
    val dates = marketData.map((a) => (str2date(a("Date").toString).getMillis.toDouble / (24 * 60 * 60 * 1000)).toInt.toDouble)
    plot(dates, prices, '-')

    val buys = orders.filter((a) => a.tags.contains("Buy"))
    val sells = orders.filter((a) => a.tags.contains("Sell"))

    val vbuy = buys.map((a) => buySellPrice(a))
    val dbuy = buys.map((a) => (str2date(a("Date").toString).getMillis.toDouble / (24 * 60 * 60 * 1000)).toInt.toDouble)

    //Buy - black
    val batch = new PlotBatch()
    batch.plota(dbuy, vbuy, '.', "black", "Buy")

    val vsell = sells.map((a) => buySellPrice(a))
    val dsell = sells.map((a) => (str2date(a("Date").toString).getMillis.toDouble / (24 * 60 * 60 * 1000)).toInt.toDouble)
    //Red - sell
    batch.plota(dsell, vsell, '+', "red", "Sell")

    orders.grouped(2).foreach((items) => {

      val buyPrice = items(0)
      val sellPrice = if (items.size > 1) items(1) else items(0)

      batch.plota(
        Seq((str2date(buyPrice("Date").toString).getMillis.toDouble / (24 * 60 * 60 * 1000)).toInt.toDouble,
          (str2date(sellPrice("Date").toString).getMillis.toDouble / (24 * 60 * 60 * 1000)).toInt.toDouble),
        Seq(buySellPrice(buyPrice), buySellPrice(sellPrice)),
        '-', if (buySellPrice(buyPrice) > buySellPrice(sellPrice)) "red" else "green")
    })
    batch.draw()

  }

  def storeResults(tags: Map[String, Any]) = {
    val state = tags ++ Map("budjet" -> budjet,
      "initialBudjet" -> initialBudjet,
      "commission" -> commisionMin,
      "parameters" -> parameters,
      "orders" -> orders)

    LocalStorage.storeDataItem("strategy_test",
      DataItem(DataItemUtil.obj2str(state), System.currentTimeMillis(), List("StrategyTest"), state))
  }
}