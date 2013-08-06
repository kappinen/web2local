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

package sources

import types.PredData
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

abstract class DataSourceJsoup extends DataSource {

  protected def parse(fetchData:(String) => Document)(url:String): PredData
  protected def parseItems(fetchData:(String) => Document)(opts:Map[String,String]): Seq[PredData]


  def gitem(criteria:String): PredData = {

    parse((url) => Jsoup.connect(url).get())(criteria)
  }

  def gitems(criteria:Map[String,String]): Seq[PredData] = {
    parseItems((url) => Jsoup.connect(url).get())(criteria)
  }

  /* For debugging */
  def ftext(url:String): String = {
    Jsoup.connect(url).get().text()
  }

}
