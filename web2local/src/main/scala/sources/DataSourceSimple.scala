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

import scala.io.Source._
import types.DataItem
import java.net.URL

abstract class DataSourceSimple extends DataSource {

  protected def parse(fetchData:(String) => String)(url:String): DataItem
  protected def parseItems(fetchData:(String) => String)(opts:Map[String,String]): Seq[DataItem]

  def ftext(url:String): String =  fromInputStream(new URL(url).openStream).getLines.mkString("\n")

  def gitem(criteria:String): DataItem = parse((url) => ftext(url))(criteria)

  def gitems(criteria:Map[String,String]): Seq[DataItem] = parseItems((url) => ftext(url))(criteria)

}