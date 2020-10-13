object RansomNoteProblem {
	/*Decide whether magazine has enough words to build ransom note using them*/
	
	private def increment(dictionary: Map[String, Int], magazine: Array[String]): Map[String, Int] = {
		magazine.foldLeft(dictionary)((dictionary, word) =>
		 dictionary updated (word, dictionary.getOrElse(word, 0) + 1))
	}

	private def decrement(dictionary: Map[String, Int], note: Array[String]): Map[String, Int] = {
		note.foldLeft(dictionary)((dictionary, word) =>
		 dictionary updated (word, dictionary.getOrElse(word, 0) - 1))
	}

	def canBuildRansomNote(magazine: Array[String], note: Array[String]): Boolean = {
		val dictionary: Map[String, Int] = Map.empty[String, Int]
		val cutOutDictionary: Map[String, Int] = decrement(increment(dictionary, magazine), note)
		val result: Boolean = cutOutDictionary.filter { case (_, amount) =>  amount < 0}.isEmpty
		result
	}

	def main(args: Array[String]): Unit = {
		val magazine: Array[String] = Array("hello", "world", "blah", "hello", "world")
		val note: Array[String] = Array("hello", "world", "world")
		println(canBuildRansomNote(magazine, note))
	}
}