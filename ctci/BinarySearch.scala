object Solution  {
	/*Binary search implementation for the array */

	def run(array: Array[Int], target: Int): Boolean = {

		def binarySearch(left: Int, right: Int): Boolean = {
			if (left > right) false			
			else {
		
				val middleIndex = (left + right) / 2
				val candidate = array(middleIndex)				
				if (left == right) candidate == target
				else {
		
					if (target == candidate) true
					else if (target < candidate) binarySearch(left, middleIndex)
					else binarySearch(middleIndex + 1, right)
				}
			} 
		}

		binarySearch(0, array.length - 1)
	}


	def main(args: Array[String]): Unit = {
		val arbitraryArray: Array[Int] = Array(5, 9 ,2, 3, 5, 1, 3, 7, 1, 9, 12)
		val target = 9
		val array: Array[Int] = arbitraryArray.sorted
		
		run(array, target) match {
			case true => println("Found target")
			case false => println("No target")
		}
	}
}