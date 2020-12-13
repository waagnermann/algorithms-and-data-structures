object FindDuplicates {

	def containsDuplicate(nums: Array[Int]): Boolean = {
		val outOfBoundIndex = nums.length

		def findDuplicates(index: Int, cached: Set[Int]): Boolean = if (index == outOfBoundIndex) false else {
			val candidate = nums(index)
			if (cached(candidate) == true) true else findDuplicates(index + 1, cached + candidate)
		} 

		findDuplicates(0, Set.empty[Int])

	}

	def main(args: Array[String]): Unit = {

		val nums: Array[Int] = Array(1,2,3,4)
		println(containsDuplicate(nums))
	}
}