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

package io


import scala.reflect.io.{File, Directory}
import types.{DataItemUtil, DataItem}
import common.Utils._
import common.Utils


object LocalStorage {

  private def getReqId(data: String): String = {
    import java.security.MessageDigest

    val bytes: Array[Byte] = MessageDigest.getInstance("MD5").digest(data.getBytes)
    bytes.map((x) => Integer.toHexString(0xFF & x)).reduce((x, y) => y.concat(x))

  }


  def storeDataItem(sourceName: String, dataItem: DataItem): DataItem = {

    thread {
      val dirName = Utils.pathResources + java.io.File.separator + sourceName;
      Directory(dirName).createDirectory(true, false)

      val writer = File(dirName + java.io.File.separator + getReqId(dataItem.source) + ".json").writer(false)

      try {
        writer.write(DataItemUtil.obj2str(dataItem))
      } finally {
        writer.close()
      }
    }

    dataItem
  }

  def selectDataItem(sourceName: String, id: String): DataItem = {
    try {
      val file = File(Utils.pathResources + java.io.File.separator
        + sourceName + java.io.File.separator
        + getReqId(id) + ".json")

      DataItemUtil.str2obj(file.slurp())
    } catch {
      case e: java.io.FileNotFoundException => null
    }
  }


  /**
   * checks if pathResources contains any directories,
   *  if not then select pathResources
   *  if only one directory select it
   *  if multiple print them out according indexes and return selected
   *
   * @return
   */
  def pathSelector(): String = {
    implicit def dirToString(x: Directory): String = x.stripExtension

    val dir = Array(Directory(".")) ++ Directory(Utils.pathResources).dirs.toArray[Directory]

    dir.size match {
      case 1 => return ""
      case 2 => return dir(1)
      case _ => {
        println("Please select source:\n")

        for (i <- dir.indices) println("  " + i + ": " + dir(i))

        try {
          return dir(readInt());
        } catch {
          case e: Exception => return pathSelector()
        }
      }
    }
  }


  @Deprecated
  def data_as[R](whatkeys: String, data: Seq[types.DataItem]): Map[String, Seq[R]] = {
    whatkeys.split(",").map((whatkey) =>
      Map(whatkey -> data.map((pred) => pred(whatkey).trim.replaceAll(",", ".").toDouble.asInstanceOf[R])))
      .reduce((a, b) => b ++ a)
  }


  def all_avail(path: String = pathSelector()): Seq[DataItem]
  = Directory(Utils.pathResources + java.io.File.separator + path)
    .files.filter((file) => file.path.endsWith(".json"))
    .toList.map((file) => DataItemUtil.str2obj(file.slurp()))


  def csvFromString(parser: (String, Array[Array[String]]) => Seq[DataItem])
                   (source: String, text: String, delimiter: String): Seq[DataItem] =
    parser(source, text.split("\n").map((line) => line.replaceAll("\r", "").replaceAll("\"", "").split(delimiter)))


  def csv(parser: (String, Array[Array[String]]) => Seq[DataItem])
         (fileName: String, delimiter: String = ";"): Seq[DataItem] =
    csvFromString(parser)(fileName, File(fileName).slurp, delimiter)


  def mini_csv(fileName: String, listName: String): Seq[String] = {
    val file = File(fileName).slurp.split("\n").map((line) => line.replaceAll("\"", "").split(","))
    val lineIndx = (file(0).zipWithIndex.filter((line) => line._1.equals(listName)))(0)._2

    file.drop(1)
      .filter((line) => line.size > lineIndx)
      .map((line) => line(1))
      .filter((line) => !line.equals(""))
      .filter((line) => !line.equals("#"))
  }


  def srcParser(source: String, data: Array[Array[String]]): Seq[DataItem] = {
    val header = data.head

    data.drop(1).map((a) => {
      val data = (header zip a).toMap
      DataItem(source, str2date(data("Date")).getMillis, List(), data)
    })
  }


  /** http://www.oanda.com/currency/historical-rates/ */
  def srcParserOanda(source: String, data: Array[Array[String]]): scala.collection.mutable.Seq[DataItem] = {
    val header = data.head

    data.drop(1).map((a) => {
      val data = (header zip a).toMap;
      DataItem(source, str2date(data("End Date").replaceAll("\"", "")).getMillis, List(), data)
    })
  }

  def srcParserNasdaqomxnordic(source: String, tdata: Array[Array[String]]): Seq[DataItem] = {
    val header = tdata.head

    def toDouble(value: String): Double = value.replaceAll(" ", "").replaceAll(",", ".").toDouble

    val filtered = tdata.drop(1).filter((a) => {
      val data = (header zip a).toMap
      isNumeric(data("Closing price")) && isNumeric(data("Low price")) && isNumeric(data("High price")) && isNumeric(data("Total volume"))
    })

    println("[+<-] Loaded : " + source + " lines " + tdata.size + " filtered:" + (tdata.size - filtered.size) + " pros:" + (1 - filtered.size / tdata.size.toDouble)*100)

    filtered.map((a) => {
      val data = (header zip a).toMap
      DataItem(source, str2date(data("Date")).getMillis, List(),
        data ++ Map(
          "Closing price" -> toDouble(data("Closing price")),
          "Volume" -> toDouble(data("Total volume")),
          "Low price" -> toDouble(data("Low price")),
          "High price" -> toDouble(data("High price"))))
    })
  }


  def srcGoogleFinance(source: String, tdata: Array[Array[String]]): Seq[DataItem] = {
    val header = tdata.head

    def toDouble(value: String): Double = value.replaceAll(" ", "").replaceAll(",", ".").toDouble

    val filtered = tdata.drop(1).filter((a) => {
      val data = (header zip a).toMap
        isNumeric(data("High price")) && isNumeric(data("Low price")) && isNumeric(data("Closing price"))
    })

    println("[+<-] Loaded : " + source + " lines " + tdata.size + " filtered:" + filtered.size + " pros:" + (1 - filtered.size / tdata.size.toDouble))

    filtered.map((a) => {
      val data = (header zip a).toMap

      DataItem(source,
        str2dateMM(data("Date")).getMillis, List(),
        data ++ Map(
          "Date" -> date2str(str2dateMM(data("Date"))),
          "Closing price" -> toDouble(data("Closing price")),
          "Low price" -> toDouble(data("Low price")),
          "High price" -> toDouble(data("High price"))))
    })
  }

}
