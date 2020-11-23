object CoinChange {
	/*You are given coins of different denominations and a total amount of money amount.
		Write a function to compute the fewest number of coins that you need to make up that amount. 
		If that amount of money cannot be made up by any combination of the coins, return -1.
		You may assume that you have an infinite number of each kind of coin.*/

	def coinChange(coins: Array[Int], amount: Int): Int = {
		val cached = scala.collection.mutable.Map.empty[Int, Int]
		
	    def run(coins: Array[Int], amount: Int): Int = {
	    	if (amount < 0) -1
	    	else if (amount == 0) 0
	    	else {
	    		var min: Int = Int.MaxValue
	    		val changes = coins map { coin =>
	    			cached.get(amount - coin) match {
	    				case Some(value) => value
	    				case None => 
	    					val res = run(coins, amount - coin)
	    					cached += (amount - coin -> res)
	    					res
	    			}
	    		}
	    		changes foreach { res => if (res >= 0 && res < min) min = res + 1 }
	    		if (min == Int.MaxValue) -1
	    		else min
	    	}
	    }

	    run(coins, amount)
    }

	def main(args: Array[String]): Unit = {
		println(s"Input: coins = [1,2,5], amount = 11, \nOutput: ${coinChange(Array(1, 2, 5), 11)}")
		println(s"Input: coins = [2], amount = 3, \nOutput: ${coinChange(Array(2), 3)}")
		println(s"Input: coins = [1], amount = 0, \nOutput: ${coinChange(Array(1), 0)}")
		println(s"Input: coins = [1], amount = 1, \nOutput: ${coinChange(Array(1), 1)}")
		println(s"Input: coins = [1], amount = 2, \nOutput: ${coinChange(Array(1), 2)}")
		println(s"Input: coins = [1, 2, 25], amount = 51, \nOutput: ${coinChange(Array(1, 2, 25), 51)}")
	}
}