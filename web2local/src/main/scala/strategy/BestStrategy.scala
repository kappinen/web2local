package strategy

import analytics.Plots._
import analytics.Plots
import analytics.Regression._
import breeze.plot.Figure
import common.Utils._
import io.LocalStorage._
import stock.MarketSignals._
import stock.MarketState
import stock.MarketStrategy._
import types.DataItem
import types.DataItemExtension._
import strategy.DefaultData


/*
 TODO: - oneliner of print buy-sell with prices and stocks and budjet recheck the calcualtions
       - chart Portfolio.returns and median * multiplier to the same frame.
 */

class BestStrategy {
  //Stategy description:
  // median slow-fast decision.

  def main(): Unit = {

    val merge = DefaultData.testData()
    //    val teststock = csv(srcParserNasdaqomxnordic)("resources/stock/NOKIA-1990-01-01-2015-08-27.csv").reverse
//        val teststock = csv(srcGoogleFinance)("resources/stock/IEO.csv", ",").reverse
        val teststock = csv(srcGoogleFinance)("resources/stock/IEO.csv", ",").reverse
//        val teststock = csv(srcGoogleFinance)("resources/stock/inda.csv", ",").reverse
//    val teststock = merge(0)


    // Median
    //    merge(0)=(7079.09, 84, 9,40,0.96, stock.MarketState@350567f1)
    //    merge(2)=(4869.39, 81, 8,38,0.96,stock.MarketState@228958a)
    //    merge(3)=(6477.38, 63, 12,43,0.96, stock.MarketState@51a5a8ba)
    //
    // Mean
    //   merge(0)=(5448.56, 90, 9,33,0.96,stock.MarketState@21b49b9a)
    //   merge(2)=(4989.2, 59, 9 31,0.93,stock.MarketState@4a467e68)
    //   merge(3)=(6850.93, 59, 12,38,0.97,stock.MarketState@1e2200ee)
    //
    // Median + volume
    //   merge(0)= (9013.99,58,11,39,0.95,stock.MarketState@4f2a0909,5,24)OMX25 series was ignored. 1760 points.
    //
    //(5064.19,116, 3,26, 0.96,stock.MarketState@7bf28545,4,17)

//    (41 - 30 + 1) * (11 - 4 + 1) * (5 - 3 + 1) * (29 - 24 + 1) * (8 - 4 + 1)

    val loops = (26 to 26).par.map((slowMedian) => {
      (3 to 4).map((fastMedian) => {
        (6 to 7).map((stopLostVariation) => {
        (17 to 19).map((slowVolume) => {
        (4 to 5).map((fastVolume) => {

          val metsoMerged2 = teststock.
            slidingBy("True price", 1)(b => {
            b.map((values) => (values("High price").toDouble + values("Low price").toDouble) / 2d).sum.toDouble
          }).slidingBy("Slow median", slowMedian)(b => {
            median(b.toDouble("True price"))
          }).slidingBy("Fast median", fastMedian)(b => {
            median(b.toDouble("True price"))
          }).slidingBy("Slow volume", slowVolume)((b) => {
            median(b.toDouble("Volume"))
          }).slidingBy("Fast volume", fastVolume)((b) => {
            median(b.toDouble("Volume"))
          })

          val strategy = testStrategy(
            Seq(sellStopLost, sellSignalMedian),
            Seq(buySignalMedian))(metsoMerged2, 10,
              new MarketState(
                Map(
                  "StopLost" -> ("0.9" + stopLostVariation) //When, sell signal is generated
                )))

          val results = strategy.report()
          (results._1, results._2, fastMedian, slowMedian, "0.9" + stopLostVariation, strategy, fastVolume, slowVolume)
        })
      }).flatten
      }).flatten
      }).flatten
    }).flatten.seq.sortBy((a) => a._1)


    loops.last._6.report()
    loops.last._6.plotData(teststock.
      slidingBy("True price", 1)(b => {
      b.map((values) => (values("High price").toDouble + values("Low price").toDouble) / 2d).sum.toDouble
    })
//      .slidingBy("Slow median", loops.last._4)(b => {
//      median(b.toDouble("True price"))
//    }).slidingBy("Fast median", loops.last._3)(b => {
//      median(b.toDouble("True price"))})
    )
    loops.last._6.report()
    loops.last._6.dumpBuysSells()


    val metsoMerged = teststock.
      slidingBy("True price", 1)(b => {
      b.map((values) => (values("High price").toDouble + values("Low price").toDouble) / 2d).sum.toDouble
    }).slidingBy("Slow median", 26)(b => {
      median(b.toDouble("True price"))
    }).slidingBy("Fast median", 3)(b => {
      median(b.toDouble("True price"))
    })
    //      .slidingBy("Slow volume", 18)((b) =>{
    //      median(b.toDouble("Volume"))
    //    }).slidingBy("Fast volume", 4)((b) =>{
    //      median(b.toDouble("Volume"))
    //    })
//    (5064.19,116, 3,26, 0.96,stock.MarketState@7bf28545,4,17)
    val metsoMerged3 = teststock.
      slidingBy("True price", 1)(b => {
      b.map((values) => (values("High price").toDouble + values("Low price").toDouble) / 2d).sum.toDouble
    }).slidingBy("Slow median", 26)(b => {
      median(b.toDouble("True price"))
    }).slidingBy("Fast median", 3)(b => {
      median(b.toDouble("True price"))
    }).slidingBy("Slow volume", 17)((b) => {
      median(b.toDouble("Volume"))
    }).slidingBy("Fast volume", 4)((b) => {
      median(b.toDouble("Volume"))
    })
    val strategy = testStrategy(
      Seq(sellStopLost, sellSignalMedian),
      Seq(buySignalMedian))(metsoMerged3, 10,
        new MarketState(
          Map(
            "StopLost" -> "0.96" //When, sell signal is generated
          ), true))

    strategy.report()
    strategy.plotData(metsoMerged)
    plotRefresh()
    plotWithDate(metsoMerged, "True price")
    plotWithDate(metsoMerged, "Volume price")
    plotWithDate(metsoMerged, "Slow median")
    plotWithDate(metsoMerged, "Fast median")
    plotNewColumn().plotWithDate(metsoMerged, "Volume")

  }

  //    && data.last("Slow volume") <= data.last("Fast volume")
  def buySignalMedian(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    data.last("Slow median").toDouble <= data.last("Fast median").toDouble &&
    data.last("Slow volume").toDouble <= data.last("Fast volume").toDouble
  }

  def sellSignalMedian(market: MarketState, data: Seq[DataItem], params: Map[String, String]): Boolean = {
    data.last("Slow median").toDouble >= data.last("Fast median").toDouble
  }

}
