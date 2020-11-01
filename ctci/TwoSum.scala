object TwoSum {
	/*Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.

	You may assume that each input would have exactly one solution, and you may not use the same element twice.

	You can return the answer in any order.

	 

	Example 1:

	Input: nums = [2,7,11,15], target = 9
	Output: [0,1]
	Output: Because nums[0] + nums[1] == 9, we return [0, 1].

	Example 2:

	Input: nums = [3,2,4], target = 6
	Output: [1,2]

	Example 3:

	Input: nums = [3,3], target = 6
	Output: [0,1]
        Below pure functional solution for the problem is provided 
	*/

	def twoSum(nums: Array[Int], target: Int): Array[Int] = {
        
                def run(map: Map[Int, Int], currentIndex: Int): Array[Int] = {
                        val complement = target - nums(currentIndex)
                        map.get(complement) match {
                                case Some(previousIndex) => Array(previousIndex, currentIndex)
                                case None => run(map updated (nums(currentIndex), currentIndex), currentIndex + 1)
                        }
                }

                val complements: Map[Int, Int] = Map.empty[Int, Int]

                run(complements, 0) 
    }

	def main(args: Array[String]): Unit = {
		println(twoSum(Array(2, 7, 11, 15), 9) mkString("[", ", ", "]"))
		// println(twoSum(Array(3, 3), 6) mkString("[", ", ", "]"))
	}
}
