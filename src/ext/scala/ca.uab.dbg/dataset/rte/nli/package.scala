package ca.uab.dbg.dataset.rte

import ca.uab.dbg.dataset.Parser

import scala.io.Source


/**
  * Created with IntelliJ IDEA.
  * User: ehsan
  * Date: 10/12/2017
  * Time: 7:21 PM
  */
package object nli {


  class NLIParser extends Parser[RTEPair] {
    override def forEach(inputFile: String, consumer: RTEPair => Unit): Unit = {
      val f = Source.fromFile(inputFile)

      for (line <- f.getLines()) {
        val tokens = line.split("\\t+")

        val label =
          if (tokens(0) == "entailment")
            "E"
          else if (tokens(0) == "contradiction")
            "C"
          else
            "N"

        val text = tokens(4).trim
        val hypo = tokens(5).trim
        val id = tokens(6)

        consumer.apply(RTEPair(id, text, hypo, label))
      }

      f.close()
    }
  }

}
