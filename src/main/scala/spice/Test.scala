package spice

import spice.http.HttpMethod

object Test {
  def main(args: Array[String]): Unit = {
    HttpMethod.all.foreach { m =>
      scribe.info(s"Method: ${m.name}")
    }
  }
}