package ca.uab.dbg

import ca.uab.dbg.dataset.rte.RTEPair
import tifmo.inference.IEngine
import tifmo.main.en._
import tifmo.onthefly.{AEngine, OnTheFly, PathAlignment}

import scala.collection.mutable

/**
  * Created with IntelliJ IDEA.
  * User: ehsan
  * Date: 10/12/2017
  * Time: 6:42 PM
  */
class Aligner(datasetName: String, inputFile: String) {
  def find(): List[(String, String)] = {
    val transformations = new mutable.ListBuffer[(String, String)]()
    dataset.execRTE(datasetName, inputFile, (pair: RTEPair) => {
      transformations ++= extract(pair.text, pair.hypo)
    })

    transformations.toList
  }

  private def extract(text: String, hypothesis: String): List[(String, String)] = {
    val allTrans = new mutable.ListBuffer[(String, String)]

    val t = normalize(text)
    val h = normalize(hypothesis)
    val (tdoc, hdoc) = parse(t, h)

    val prem = tdoc.makeDeclaratives
    val hypo = hdoc.makeDeclaratives

    val ie = new IEngine

    prem.flatMap(_.toStatements).foreach(ie.claimStatement)
    hypo.flatMap(_.toStatements).foreach(ie.checkStatement)

    val res = new EnResources
    val words = tdoc.allContentWords[EnWord] ++ hdoc.allContentWords[EnWord]
    for (s <- words.subsets(2)) {
      val a = s.head
      val b = (s - a).head
      if (res.synonym(a, b)) {
//        System.err.println("synonym: " + a + " = " + b)
        ie.subsume(a, b)
        ie.subsume(b, a)
      } else if (Set(a.mypos, b.mypos).subsetOf(Set("J", "R")) && res.antonym(a, b)) {
//        System.err.println("antonym: " + a + " <> " + b)
        ie.disjoint(a, b)
      } else {
        if (res.hyponym(a, b) && !b.isNamedEntity) {
//          System.err.println("hyponym: " + a + " -> " + b)
          ie.subsume(a, b)
        }
        if (res.hyponym(b, a) && !a.isNamedEntity) {
//          System.err.println("hyponym: " + b + " -> " + a)
          ie.subsume(b, a)
        }
      }
    }

    val proven = (x:IEngine) => hypo.flatMap(_.toStatements).forall(x.checkStatement)

    val ae = new AEngine(prem)
    hypo.foreach(ae.addGoal)

    val otf = new OnTheFly(ie, ae)

    val sim = new EnSimilarityMikolov13(res, 0.7f)

    val score = (x: PathAlignment) => {
      // evaluate path alignment

      val PathAlignment(psub, psup) = x
      if (psup.rnrs.exists(x => x._1 == TIME || x._3 == TIME)
        && !psub.rnrs.exists(x => x._1 == TIME || x._3 == TIME)) {
        // time role unmatch, filtered.
        0.0
      } else {
        val wsub = psub.rnrs.map(x => x._2.token.getWord.asInstanceOf[EnWord]).filter(!_.isStopWord)
        val wsup = psup.rnrs.map(x => x._2.token.getWord.asInstanceOf[EnWord]).filter(!_.isStopWord)
        if (wsub.isEmpty || wsup.isEmpty) {
          // filtered.
          0.0
        } else if (wsup.exists(x => (x.isNamedEntity || x.mypos == "D")
          && !wsub.exists(y => res.synonym(y, x) || res.hyponym(y, x)))) {
          // time or named entity unmatch, filtered.
          0.0
        } else {
          // phrase similarity
          sim.similarity(wsub, wsup)
        }
      }
    }

    val (ret, rec) = otf.tryKnowledge(score, 0.1, proven)

    if (ret) {

      if (rec.isEmpty) {

//        System.err.println("Proven.")
//        println(id + "," + task + "," + gold_label + ",1.0")

      } else {

//        System.err.println("Proven with on-the-fly knowledge.")
//        println(id + "," + task + "," + gold_label + "," + rec.last._2)

        var necessary = Nil: List[(String, String, Double)]

        var tmpie = new IEngine
        tmpie.load(ie.dump())

        def loop() {
          val bak = tmpie.dump()
          rec.find(x => {
            x._1.foreach(_._2.toStatements.foreach(tmpie.claimStatement(_)))
            proven(tmpie)
          }) match {
            case Some((algs, scr)) => {
              val ss = for ((x, y) <- algs) yield {
                val sub = for ((r1, n, r2) <- x.subPath.rnrs) yield {
                  val pre = r1.toString + "(" + n.token.getWord + ")"
                  if (r2 == null) pre else (pre + r2)
                }
                val sup = for ((r1, n, r2) <- x.supPath.rnrs) yield {
                  val pre = r1.toString + "(" + n.token.getWord + ")"
                  if (r2 == null) pre else (pre + r2)
                }
                (sub.mkString("", "-", ""), sup.mkString("", "-", ""))
              }
              for ((sub, sup) <- ss) necessary = (sub, sup, scr) :: necessary
              tmpie = new IEngine
              tmpie.load(bak)
              algs.foreach(_._2.toStatements.foreach(tmpie.claimStatement(_)))
              if (!proven(tmpie)) loop()
            }
            case None => throw new Exception("this should not happen")
          }
        }

        loop()

//        System.err.println("ON THE FLY:")

        val wordPattern = "\\((\\w+)_\\w\\)".r

        val trans = necessary
          .filter(t => {
            val sub = t._1
            val sup = t._2

            val bw = (for (b <- wordPattern.findAllMatchIn(sub)) yield b.group(1)).mkString(",")
            val pw = (for (p <- wordPattern.findAllMatchIn(sup)) yield p.group(1)).mkString(",")

            sub != sup && !sub.contains(sup) && !sup.contains(sub) && !bw.contains(pw) && !pw.contains(bw)
          })
          .map(t => (t._1, t._2))
        allTrans ++= trans

//        System.err.println("ALIGNED: " + necessary.size + " / ACCEPTED: " + trans.size)

        for ((sub, sup) <- trans) {
//          System.err.println(" score: " + scr)
          println("  subPath: " + sub + " - supPath: " + sup + "\n")
        }

        System.err.println("-------------------")

      }
    }

    allTrans.toList
  }
}

object Aligner extends App {
  if (args.length < 2) {
    System.err.println("not enough arguments")
    System.exit(1)
  } else {
    val aligner = new Aligner(args(0), args(1))
    aligner.find()
  }

  sys.exit(0)

}
