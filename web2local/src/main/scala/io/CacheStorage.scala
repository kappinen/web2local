package io

import scala.reflect.io.File


/**
 * Created by home on 30.8.2016.
 */
object CacheStorage {
  val BASEPATH=File("/tmp/web2local").createDirectory(true, false)
  var CACHE_ENABLED=true

  def getCache(tag: String): String = {
    if (!CACHE_ENABLED) {
      return ""
    }

    val filename = LocalStorage.md5sum(tag) + ".cache"

    if (BASEPATH.files.filter(a=> a.name.equals(filename)).size == 1) {
      println("[CacheStorage] fetching:" + tag + " cache:" + (BASEPATH.toAbsolute.path + "/" + filename))
      File(BASEPATH.toAbsolute.path + "/" + filename).slurp()
    } else {
      ""
    }
  }

  def putCache(tag: String, data: String): String = {
    if (!CACHE_ENABLED) {
      return data
    }

    val writer = File(BASEPATH.toAbsolute.path + "/" + LocalStorage.md5sum(tag) + ".cache").writer(false)
    writer.write(data)
    writer.flush()
    data
  }
}
