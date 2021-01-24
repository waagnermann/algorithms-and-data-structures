object SearchInRotatedSortedArray {
	def searchRecursive(nums: Array[Int], target: Int, currentIndex: Int): Int = {
		println(nums mkString("[", ", ", "]"))
		if (nums.length == 1) {
			if (nums(0) == target) currentIndex else -1
		} else {
			val first = nums(0)
			val last = nums(nums.length - 1)
			val middleIndex = nums.length / 2
			val middle = nums(middleIndex)

			if (first <= last) {
				if (target < middle) searchRecursive(nums.take(middleIndex), target, currentIndex) 
				else searchRecursive(nums.drop(middleIndex), target, currentIndex + middleIndex)
			} else {
				if (target < middle) {
					if (target < first) {
						if (first < middle) searchRecursive(nums.drop(middleIndex), target, currentIndex + middleIndex)
						else searchRecursive(nums.take(middleIndex), target, currentIndex)
					} else searchRecursive(nums.take(middleIndex), target, currentIndex) 
				} else {
					if (target >= first) {
						if (middle > first) searchRecursive(nums.drop(middleIndex), target, currentIndex + middleIndex)
						else searchRecursive(nums.take(middleIndex), target, currentIndex)
					} else searchRecursive(nums.drop(middleIndex), target, currentIndex + middleIndex)
				}
			}
		}
	}


	def search(nums: Array[Int], target: Int): Int = {
		val currentIndex = 0
		searchRecursive(nums, target, currentIndex)
	}


	def main(args: Array[String]): Unit = {
		val (input, target) = (Array(4, 5, 6, 7, 0, 1, 2), 0)
		val output = search(input, target)
		println(s"input: ${input mkString("[", ", ", "]")}, target: $target")
		println(s"output: $output")
	}
}