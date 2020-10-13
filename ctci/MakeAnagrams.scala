object MakeAnagrams {
	/*You have to strings both in lowercase only. How many letters should you remove
	 in both strings to make them anagrams?*/

	final private val AlphabetLength: Int = 26
	final private val AlphabetHead: Char = 'a'

	private def charFrequencyArray(word: String) : Array[Int] = {
		val array : Array[Int] = Array.ofDim[Int](AlphabetLength)
		word foreach { letter => 
			val index : Int = letter - AlphabetHead
			array(index) += 1 }
		array
	}

	def charsToRemoveCount(leftString: String, rightString: String): Int = {
		val leftArray: Array[Int] = charFrequencyArray(leftString)
		val rightArray: Array[Int] = charFrequencyArray(rightString)

		val zipped: Array[Tuple2[Int, Int]] = leftArray zip rightArray
		val sumDifference: Int = zipped.map { case (left, right) => (left - right).abs }.sum
		
		sumDifference
	}

	def main(args: Array[String]): Unit = {
		val leftWord: String = "hello"
		val rightWord: String = "billion"

		println(s"To make those two strings anagrams you have to remove ${charsToRemoveCount(leftWord, rightWord)} chars")
	}
	
}