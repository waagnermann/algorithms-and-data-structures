import scala.collection.mutable.ArrayBuffer

object FindSubstringProblem {
	
	private def computePi(i: Int, j: Int, rawPi: ArrayBuffer[Int], pattern: String, breakPoint: Int): ArrayBuffer[Int] = i == breakPoint match {
		case true => rawPi
		case false => {
			if (pattern(i) == pattern(j)) computePi(i + 1, j + 1, rawPi += j + 1, pattern, breakPoint)
			else {
				if (j == 0) computePi(i + 1, j, rawPi += 0, pattern, breakPoint)
				else computePi(i, rawPi(j - 1), rawPi, pattern, breakPoint)
			}
		}
	}

	private def getLps(pattern: String): ArrayBuffer[Int] = {
		val substringSize = pattern.length
		val pi = ArrayBuffer(0)
		computePi(1, 0, pi, pattern, substringSize)
	}


	def kmp(text: String, pattern: String): Unit = {
		val lps = getLps(pattern)
		val textSize = text.length
		val patternSize = pattern.length 

		def computeKmp(k: Int, j: Int): Unit = k == textSize match {
			
			case true => j == patternSize match {
				case true => {
					println(s"Found pattern at ${textSize - patternSize} index")
					println("Work is done!")
				}
				case false => println("Work is done!")
			} 

			case false => j == patternSize match {
				case true => {
					println(s"Found pattern at ${k - j} index")
					computeKmp(k, lps(j - 1))
				}
				case false => {
					if (text(k) == pattern(j)) computeKmp(k + 1, j + 1)
					else {
						if (j == 0) computeKmp(k + 1, j)
						else computeKmp(k, lps(j - 1))
					}
				}
			}
		}

		computeKmp(0, 0)

	}

	def main(args: Array[String]): Unit = {
		// kmp("abcabeabcabcabd", "abcabd")
		// kmp("ABABDABACDABABCABAB", "ABABCABAB")
		kmp("AABAACAADAABAABA", "AABA")
	}
}