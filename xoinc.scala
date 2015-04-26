import scala.language.postfixOps
object xoinc {
   var MaxMoney:Map[(Int,List[Int],Int), BigInt] = Map()
   
   def main(args: Array[String]) = {

     //read in file 
     val raw_input = io.Source.stdin.getLines

     //make list with all but first character
     val stack = raw_input.toList.tail.map(x => x.toInt)

     //solve
     MaxMoney += (
         (2,stack, stack.length) -> maxMoney(2,stack, stack.length)
     )
     println( MaxMoney.apply((2,stack, stack.length)) )
   }
   
   /*
    * @param numMax - max amount of coins player can choose
    * @param stack - a list representing the stack
    */
   def maxMoney(numMax: Int, stack: List[Int], stackLen: Int): BigInt = {
     
     if (MaxMoney.contains((numMax,stack,stackLen)))
       MaxMoney.apply((numMax,stack,stackLen))
     else{
       MaxMoney += (
         (numMax,stack,stackLen) -> {
           val numCoins = List.range(1, numMax+1)
           //make list of value of coins selected + nextTurn value
           val playerOutcomes = numCoins.map(x => (nextTurn(x, stack drop x, stackLen-x) + stackSum(stack,x)))
           playerOutcomes max
         })
       MaxMoney.apply((numMax,stack,stackLen))  
     }
   }
       
   //returns max value of user's next turn
   def nextTurn(numCoins: Int, stack: List[Int], stackLen: Int): BigInt = {
     if (stackLen < numCoins*2)
       0 //opponent will take all remaining coins
     else{
       val opponentCoinChoices = List.range(1, numCoins*2+1)
       val playerOutcomes = opponentCoinChoices.map(x => maxMoney(2*x, stack drop x, stackLen-x))
       if (playerOutcomes.isEmpty) 0 else playerOutcomes min
       //opponent makes move minimizing player's next sum
     }  
   } 
   
   //gets sum of first n coins on stack
   def stackSum(stack: List[Int], n: Int): BigInt = stack.take(n).sum
       
}