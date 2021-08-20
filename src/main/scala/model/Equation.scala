package calculator.model

import scala.math.BigDecimal

//describe an equation which has 2 numbers and 1 operation 
abstract class Equation{

     var number1: BigDecimal
     var number2: BigDecimal
     var operation: String

     //an equation can be executed and return an answer 
     def executeEquation(): BigDecimal 
    
}    