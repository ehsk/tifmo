package mylib

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.POS
import edu.mit.jwi.item.Pointer
import edu.mit.jwi.item.IWordID
import edu.mit.jwi.item.IWord
import edu.mit.jwi.item.ISynsetID
import edu.mit.jwi.item.ISynset
import edu.mit.jwi.morph.WordnetStemmer

import scala.collection.JavaConversions._

package res.en {
	
	object EnWordNet {
		
		private[this] val dir = EnWordNet.getClass.getClassLoader.getResource("en/dict").toURI
		
		private[this] val dict = new Dictionary(new java.io.File(dir))
		dict.open()
		
		type SemEntry = ISynset
		
		type WNRelation = Pointer
		
		object WNRelation {
			
			val ANTONYM = Pointer.ANTONYM
			
			val HYPERNYM = Pointer.HYPERNYM
			
			val HYPERNYM_INSTANCE = Pointer.HYPERNYM_INSTANCE
			
			val HOLONYM_MEMBER = Pointer.HOLONYM_MEMBER
			
			val HOLONYM_PART = Pointer.HOLONYM_PART
			
			val HOLONYM_SUBSTANCE = Pointer.HOLONYM_SUBSTANCE
			
			val HYPONYM = Pointer.HYPONYM
			
			val HYPONYM_INSTANCE = Pointer.HYPONYM_INSTANCE
			
			val MERONYM_MEMBER = Pointer.MERONYM_MEMBER
			
			val MERONYM_PART = Pointer.MERONYM_PART
			
			val MERONYM_SUBSTANCE = Pointer.MERONYM_SUBSTANCE
			
			val ENTAILMENT = Pointer.ENTAILMENT
			
			val ATTRIBUTE = Pointer.ATTRIBUTE
			
			val DERIVATIONALLY_RELATED = Pointer.DERIVATIONALLY_RELATED
			
			val DERIVED_FROM_ADJ = Pointer.DERIVED_FROM_ADJ
			
			val PERTAINYM = Pointer.PERTAINYM
		}
		
		private[this] val stemmer = new WordnetStemmer(dict)
		private[this] val allpos = POS.values.toSet
		
		private[this] val lock = new AnyRef
		
		def hasWord(s: String) = lock.synchronized {
			if (s.isEmpty) {
				false
			} else {
				allpos.exists(x => stemmer.findStems(s, x).exists(y => dict.getIndexWord(y, x) != null))
			}
		}
		
		def stem(s: String, mypos: String) = lock.synchronized {
			val tmp = mypos match {
				case "J" => stemmer.findStems(s, POS.ADJECTIVE).toSet
				case "N" => stemmer.findStems(s, POS.NOUN).toSet
				case "R" => stemmer.findStems(s, POS.ADVERB).toSet
				case "V" => stemmer.findStems(s, POS.VERB).toSet
				case _ => Set.empty[String]
			}
			if (tmp.isEmpty) s else {
				val minlen = tmp.map(_.length).min
				tmp.filter(_.length == minlen).min
			}
		}
		
		def synsets(lemma: String, mypos: String) = lock.synchronized {
			
			val stems = mypos match {
				case "J" => stemmer.findStems(lemma, POS.ADJECTIVE).map((POS.ADJECTIVE, _)).toSet
				case "N" => stemmer.findStems(lemma, POS.NOUN).map((POS.NOUN, _)).toSet
				case "R" => stemmer.findStems(lemma, POS.ADVERB).map((POS.ADVERB, _)).toSet
				case "V" => stemmer.findStems(lemma, POS.VERB).map((POS.VERB, _)).toSet
				case _ => allpos.flatMap(x => stemmer.findStems(lemma, x).map((x, _)))
			}
			
			for ((p, s) <- stems; idxw = dict.getIndexWord(s, p); if idxw != null; wid <- idxw.getWordIDs) yield {
				dict.getWord(wid).getSynset
			}
		}
		
		def getLemmas(ss: Set[SemEntry]) = lock.synchronized {
			ss.flatMap(_.getWords.map(_.getLemma))
		}
		
		def lexical(ss: Set[SemEntry], rel: WNRelation) = lock.synchronized {
			val ws = ss.flatMap(_.getWords)
			val rws = ws.flatMap(_.getRelatedWords(rel))
			rws.map(dict.getWord(_).getSynset)
		}
		
		def semantic(ss: Set[SemEntry], rel: WNRelation) = lock.synchronized {
			val rss = ss.flatMap(_.getRelatedSynsets(rel))
			rss.map(dict.getSynset(_))
		}
		
	}
	
}

