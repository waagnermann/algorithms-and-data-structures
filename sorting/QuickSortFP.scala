object QuickSortFP extends App {
    val arr = Array(3, 7, 8, 5, 2, 1, 9, 5, 4)

    val sortedArray = quickSort(arr)

    // the fp/recursive algorithm
    def quickSort(xs: Array[Int]): Array[Int] = {
        if (xs.length <= 1) xs
        else {
            val pivot = xs(xs.length - 1)
            Array.concat(
                quickSort(xs filter (_ < pivot)),
                xs filter (_ == pivot),
                quickSort(xs filter (_ > pivot))
                )
        }
    }

    println(sortedArray.toList)
}