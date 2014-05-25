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


  def writePredData(sourceName: String, predData: DataItem): DataItem = {

    thread {
      val dirName = Utils.pathResources + java.io.File.separator + sourceName;
      Directory(dirName).createDirectory(true, false)

      val writer = File(dirName + java.io.File.separator + getReqId(predData.source) + ".json").writer(false)

      try {
        writer.write(DataItemUtil.obj2str(predData))
      } finally {
        writer.close()
      }
    }

    predData
  }


  def getType(x:Any) : Seq[DataItem] = x match {
    case b : DataItem => Seq(b)
    case _ => x.asInstanceOf[Seq[DataItem]]
  }


  def writePredData2(sourceName: String, predData: Any): Seq[DataItem] = {

    val toProcess = getType(predData)

    thread {
      val dirName = Utils.pathResources + java.io.File.separator + sourceName
      Directory(dirName).createDirectory(true, false)
      val writer = File(dirName + java.io.File.separator + getReqId(sourceName) + ".json").writer(false)

      try {
          toProcess.map((line) => writer.write(DataItemUtil.obj2str(line)))
      } finally {
        writer.close()
      }
    }

    toProcess
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


  def data_as[R](whatkeys: String, data: Seq[types.DataItem]): Map[String, Seq[R]] = {
    whatkeys.split(",").map((whatkey) =>
      Map(whatkey -> data.map((pred) => pred.data(whatkey).toString().trim.replaceAll(",",".").toDouble.asInstanceOf[R])))
      .reduce((a, b) => b ++ a)
  }


  def all_avail(path: String = pathSelector()): Seq[DataItem]
      = Directory(Utils.pathResources + java.io.File.separator + path)
        .files.filter((file) => file.path.endsWith(".json"))
        .toList.map((file) => DataItemUtil.str2obj(file.slurp()))


  def getPredData(sourceName: String, id: String): DataItem = {
    try {
      val file = File(Utils.pathResources + java.io.File.separator
                      + sourceName + java.io.File.separator
                      + getReqId(id) + ".json")

      DataItemUtil.str2obj(file.slurp())
    } catch {
      case e: java.io.FileNotFoundException => null
    }
  }


  def defParser(source: String, data: Array[Array[String]]): Seq[DataItem] = {
    val header = data.head;

    data.drop(1).map((a) => {
      val data = (header zip a).toMap;
      DataItem(source, str2date(data("Date")).getMillis, List(), data)
    })
  }


  def csvFromString(parser:(String, Array[Array[String]]) => Seq[DataItem])
                   (source:String, text:String, delimiter: String) : Seq[DataItem] =
    parser(source, text.split("\n").map((line) => line.replaceAll("\r","").replaceAll("\"","").split(delimiter)))


  def csv(parser:(String, Array[Array[String]]) => Seq[DataItem])
         (fileName:String, delimiter: String = ";") : Seq[DataItem] =
    csvFromString(parser)(fileName, File(fileName).slurp, delimiter)


  def mini_csv(fileName:String, listName: String) : Seq[String] = {
    val file = File(fileName).slurp.split("\n").map((line) => line.replaceAll("\"","").split(","))
    val lineIndx = (file(0).zipWithIndex.filter((line) => line._1.equals(listName)))(0)._2

    file.drop(1)
      .filter((line) => line.size > lineIndx)
      .map((line) => line(1))
      .filter((line) => !line.equals(""))
      .filter((line) => !line.equals("#"))
  }


  /**  http://www.oanda.com/currency/historical-rates/ */
  def defParserOanda(source: String, data: Array[Array[String]]): scala.collection.mutable.Seq[DataItem] = {
    val header = data.head;

    data.drop(1).map((a) => {
      val data = (header zip a).toMap;
      DataItem(source, str2date(data("End Date").replaceAll("\"", "")).getMillis, List(), data)
    })
  }
}
