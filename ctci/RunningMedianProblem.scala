import scala.collection.mutable.PriorityQueue

object RunningMedianProblem extends App {
	class Solution {
		
		private def getSmallerHeap(left: PriorityQueue[Int], right: PriorityQueue[Int]): PriorityQueue[Int] = {
			if (left.sizeCompare(right.size) == -1) {
				left
			} else {
				right
			}
		}

		private def getBiggerHeap(left: PriorityQueue[Int], right: PriorityQueue[Int]): PriorityQueue[Int] = {
			if (left == getSmallerHeap(left, right)) {
				right
			} else {
				left
			}
		}

		private def addNumber(number: Int, lowers: PriorityQueue[Int], highers: PriorityQueue[Int]): Unit = {
			if (lowers.isEmpty || number < lowers.head) {
				lowers.addOne(number)
			} else {
				highers.addOne(number)
			}
		}

		private def rebalance(lowers: PriorityQueue[Int], highers: PriorityQueue[Int]): Unit= {
			val smallerHeap: PriorityQueue[Int] = getSmallerHeap(lowers, highers)
			val biggerHeap: PriorityQueue[Int] = getBiggerHeap(lowers, highers)

			if (biggerHeap.size - smallerHeap.size >= 2) {
				smallerHeap.addOne(biggerHeap.dequeue)
			}
		}

		private def calculateMedian(lowers: PriorityQueue[Int], highers: PriorityQueue[Int]) = {
			val smallerHeap: PriorityQueue[Int] = getSmallerHeap(lowers, highers)
			val biggerHeap: PriorityQueue[Int] = getBiggerHeap(lowers, highers)

			if (smallerHeap.size == biggerHeap.size) {
				val leftMiddle: Int = lowers.head
				val rightMiddle: Int = highers.head
				(leftMiddle + rightMiddle).toDouble / 2
			} else {
				biggerHeap.head
			}
		}

		private def getMedian(number: Int, lowers: PriorityQueue[Int], highers: PriorityQueue[Int]) = {
			addNumber(number, lowers, highers)
			rebalance(lowers, highers)
			calculateMedian(lowers, highers)
		}

		def getMedians(array: Array[Int]): Array[Double] = {
			val highers: PriorityQueue[Int] = PriorityQueue[Int]()(Ordering[Int].reverse)
			val lowers: PriorityQueue[Int] = PriorityQueue[Int]()(Ordering[Int])

			array map (number => getMedian(number, lowers, highers))
		}
	}

	val solution: Solution = new Solution
	val array: Array[Int] = Array(5, 15, 1, 3)
	val medians: Array[Double] = solution.getMedians(array)
	println(medians.toList)
}