import scala.math.Ordered
import scala.reflect.ClassTag

object SortWithComparator {

	case class Player(name: String, points: Long) extends Ordered[Player] {
		def compare(that: Player) = if (this.points != that.points) this.points compare that.points else that.name compare this.name
	}

	def quickSort[T <: Ordered[T] : ClassTag](xs: Array[T]): Array[T] = {
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

	def main(args: Array[String]): Unit = {
		val array: Array[Player] = Array(
			Player("Sarah", 400),
			Player("John", 1971),
			Player("Gayle", 1903),
			Player("Davis", 1400),
			Player("Andy", 1400),
			Player("Tobin", 1400)
		)
		val sortedArray: Array[Player] = quickSort(array)
		println(sortedArray mkString ", ")
	}
}
