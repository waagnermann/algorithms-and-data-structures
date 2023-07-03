object AnagramsProblem {
	/* Given two strings define whether they anagrams or not*/
	def countLetters(string: String): Map[Char, Int] = {
		string.foldLeft(Map.empty[Char, Int])((map, letter) => map updated (letter.toLower, map.getOrElse(letter.toLower, 0) + 1))
  	}

	def areAnagrams(left: String, right: String): Boolean = if (left.length != right.length) false else {
	  	countLetters(left) == countLetters(right)
	}

	def charsToRemoveCount(leftMap: Map[Char, Int], rightMap: Map[Char, Int]): Int = {
		var amount = 0

		leftMap.foreach { case (char, leftFreq) =>
		  val incrementor = rightMap.get(char) match {
		    case Some(rightFreq) => 0
		    case None => leftFreq
		  }
		  amount += incrementor
		}

		rightMap.foreach { case (char, rightFreq) =>
		  val incrementor = leftMap.get(char) match {
		    case Some(leftFreq) => (rightFreq - leftFreq).abs
		    case None => rightFreq
		  }
		  amount += incrementor
		}

		amount
	}

	def main(args: Array[String]): Unit = {
		println(areAnagrams("elements", "emelstne")) // true
		println(areAnagrams("", "")) // true
		println(areAnagrams("final", "fnial")) // true
		println(areAnagrams("baabs", "abba")) // false
		println(areAnagrams("anagram", "aanagramm")) // false
	    println(areAnagrams("proxy", "porxy")) // true
	    println(areAnagrams("silent", "listen")) // true
	    println(areAnagrams("fried", "fired")) // true
	    println(areAnagrams("", "word")) // false
	    println(areAnagrams("Capital", "capital")) // true
	    println(areAnagrams("definition", "DEFINITION")) // true
	    println(areAnagrams("fence", "fEnCe")) // true
	    println(areAnagrams("someThing", "")) // false
	    println(areAnagrams("database", "EdaBAatS")) // true
		}
}
