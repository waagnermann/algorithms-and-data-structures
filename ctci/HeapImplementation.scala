object HeapImplementation extends App {
	class MinIntHeap {
		private var capacity : Int = 10
		private var size : Int = 0
		private var items: Array[Int] = Array.ofDim[Int](capacity)

		private def getLeftChildIndex(parentIndex: Int): Int = parentIndex * 2 + 1
		private def getRightChildIndex(parentIndex: Int): Int = parentIndex * 2 + 2
		private def getParentIndex(childIndex: Int): Int = (childIndex - 1) / 2

		private def hasLeftChild(index: Int): Boolean = getLeftChildIndex(index) < size
		private def hasRightChild(index: Int): Boolean = getRightChildIndex(index) < size
		private def hasParent(index: Int): Boolean = getParentIndex(index) >= 0

		private def leftChild(index: Int) = items(getLeftChildIndex(index))
		private def rightChild(index: Int) = items(getRightChildIndex(index))
		private def parent(index: Int) = items(getParentIndex(index))

		private def swap(indexOne: Int, indexTwo: Int): Unit = {
			val temp: Int = items(indexOne)
			items(indexOne) = items(indexTwo)
			items(indexTwo) = temp
		}

		private def ensureExtraCapacity(): Unit = {
			if (size == capacity) {
				items = Array.copyOf(items, capacity * 2)
				capacity *= 2
			}
		}

		private def heapifyUp(): Unit = {
			var index: Int = size - 1
			while (hasParent(index) && parent(index) > items(index)) {
				swap(getParentIndex(index), index)
				index = getParentIndex(index)
			}
		}

		private def heapifyDown(): Unit = {
			var index: Int = 0
			var done: Boolean = false
			while (hasLeftChild(index) && !done) {
				val smallerChildIndex: Int = if (rightChild(index) < leftChild(index)) {
					getRightChildIndex(index)
					} else getLeftChildIndex(index)

				if (items(index) < items(smallerChildIndex)) {
					done = true
				} else {
					swap(index, smallerChildIndex)
				}

				index = smallerChildIndex
			}
		}

		def peek(): Int = if (size == 0) {
			val message: String = "Heap is empty"
			throw new IllegalStateException(message)
		} else items(0)

		def poll(): Int = if (size == 0) {
			val message: String = "Heap is empty"
			throw new IllegalStateException(message)
		} else {
			val item: Int = items(0)
			items(0) = items(size - 1)
			size -= 1
			heapifyDown()
			item
		}

		def add(item: Int) = {
			ensureExtraCapacity()
			items(size) = item
			size += 1
			heapifyUp()
		}
	}

	val minIntHeap: MinIntHeap = new MinIntHeap
	minIntHeap.add(25)
	minIntHeap.add(10)
	minIntHeap.add(15)
	minIntHeap.add(17)
	minIntHeap.add(20)
	minIntHeap.add(3)
	println(minIntHeap.poll())
	println(minIntHeap.poll())
	println(minIntHeap.poll())
	println(minIntHeap.poll())
	println(minIntHeap.poll())
	println(minIntHeap.poll())
}
