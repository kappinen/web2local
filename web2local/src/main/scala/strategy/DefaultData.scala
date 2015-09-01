package strategy

import common.Utils._
import io.LocalStorage._
import types.{DataItemUtil, DataItem}

/**
 * Created by home on 27.8.2015.
 */
object DefaultData {

  //Test time-series
  def testData(): Seq[Seq[DataItem]] = {
    DataItemUtil.mergeBy((a: DataItem, b: DataItem) => a("Date").equals(b("Date")))(
      csv(srcParserNasdaqomxnordic)("resources/stock/MEO1V_FI0009007835-1990-01-01-2013-08-10.csv").reverse, //metso
      csv(srcParserNasdaqomxnordic)("resources/stock/FI0008900212-2000-01-03-2014-08-01.csv").reverse, //omx
      csv(srcParserNasdaqomxnordic)("resources/stock/KESBV-1990-01-01-2014-08-29.csv").reverse,//kesko
      csv(srcParserNasdaqomxnordic)("resources/stock/SAMAS_FI0009003305-1990-01-01-2013-08-10.csv").reverse, //sampo
      csv(srcParserNasdaqomxnordic)("resources/stock/NOKIA-1990-01-01-2015-08-27.csv").reverse, //nokia
      csv(srcGoogleFinance)("resources/stock/IEO.csv", ",").reverse) //IEO
  }

}
