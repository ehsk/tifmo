package ca.uab.dbg.dataset.rte

import ca.uab.dbg.dataset.Parser

/**
  * Created with IntelliJ IDEA.
  * User: ehsan
  * Date: 10/12/2017
  * Time: 7:10 PM
  */
package object pascal {

  class PascalParser extends Parser[RTEPair] {
    override def forEach(inputFile: String, consumer: RTEPair => Unit): Unit = {
      val f = xml.XML.loadFile(inputFile)

      for (p <- f \ "pair") {

        val id = (p \ "@id").text

        val goldLabel = if (
          (p \ "@value").text == "TRUE"
            || (p \ "@entailment").text == "ENTAILMENT"
            || (p \ "@entailment").text == "YES"
        ) {
          "Entailment"
        } else {
          "NoEntailment"
        }

        val text = (p \ "t").text.trim
        val hypo = (p \ "h").text.trim
        consumer.apply(RTEPair(id, text, hypo, goldLabel))
      }
    }
  }
}
