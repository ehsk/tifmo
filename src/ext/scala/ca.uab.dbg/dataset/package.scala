package ca.uab.dbg

import ca.uab.dbg.dataset.rte.RTEPair
import ca.uab.dbg.dataset.rte.nli.NLIParser
import ca.uab.dbg.dataset.rte.pascal.PascalParser

/**
  * Created with IntelliJ IDEA.
  * User: ehsan
  * Date: 10/12/2017
  * Time: 7:09 PM
  */
package object dataset {

  trait Parser[R <: Row] {
    def forEach(inputFile: String, consumer: R => Unit)
  }

  trait Row

  def execRTE(datasetName: String, inputFile: String, consumer: (RTEPair => Unit)): Unit = {
    datasetName.toLowerCase match {
      case "pascal" => new PascalParser().forEach(inputFile, consumer)
      case "snli" | "multinli" => new NLIParser().forEach(inputFile, consumer)
      case _ => System.err.println("Unrecognized dataset")
    }
  }
}
