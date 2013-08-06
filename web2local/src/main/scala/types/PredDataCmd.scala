package types

import com.codahale.jerkson.Json._
import scala._

object PredDataCmd {


  def obj2str(data: PredData):String = {
    generate(data)
  }


  def str2obj(objectdata: String):PredData = {
    inClassLoader(classOf[PredData]) ({
      parse[PredData](objectdata)
    })
  }


  //Reference:   https://github.com/codahale/jerkson/issues/38
  private def inClassLoader[T](cls: Class[_])(f: => T): T = {
    val prev = Thread.currentThread.getContextClassLoader
    try {
      Thread.currentThread.setContextClassLoader(cls.getClassLoader)

      f
    } finally {
      Thread.currentThread.setContextClassLoader(prev)
    }
  }

}
