package stock

import types.DataItem

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
object MarketStrategy {

  def testStrategy(sellSignals: Seq[(MarketState, Seq[DataItem], Map[String, String]) => Boolean],
                   buySignals: Seq[(MarketState, Seq[DataItem], Map[String, String]) => Boolean])
                  (data: Seq[DataItem], minSize: Int, market: MarketState): MarketState = {
    val mResult: MarketState = {
      if (data.size <= minSize) {
        market;
      } else {
        testStrategy(sellSignals, buySignals)(data.dropRight(1), minSize, market)
      }
    }

    val sell = sellSignals.par.map((signal) => signal(mResult, data, market.params)).reduce((a, b) => a || b)
    if (sell) market.sell(data.last)

    val buy = buySignals.par.map((signal) => signal(mResult, data, market.params)).reduce((a, b) => a && b)
    if (!sell && buy) market.buy(data.last)

    return mResult
  }


  def buySellPrice(item: DataItem): Double = ((item.toDouble("High price") + item.toDouble("Low price")) / 2.toDouble + item.toDouble("Closing price")) / 2.toDouble
}
