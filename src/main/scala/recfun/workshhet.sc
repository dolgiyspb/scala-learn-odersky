def makeChange(sum: Int, coins: List[Int]): Int = {
  if (sum == 0) 1
  else if(sum < 0) 0
  else if (coins.isEmpty) 0
  else makeChange(sum - coins.head, coins) + makeChange(sum, coins.tail)
}

makeChange(4, List(1, 2))
makeChange(301,List(5,10,20,50,100,200,500))
makeChange(300,List(5,10,20,50,100,200,500))