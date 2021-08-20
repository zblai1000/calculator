package calculator.model

import scala.math.BigDecimal

//Unary equation is an equation that has 1 value 
class UnaryEquation(num: BigDecimal, operationSelected: String) extends Equation {

    var number1 = 0

    //since unary equation only has 1 value, number2 is set to 0 
    var number2 = num
    var operation = operationSelected

    //return an answer based on the operator passed into the equation 
    override def executeEquation(): BigDecimal = {

        operation match {

            case "+" => return number1 + number2 
            case "-" => return number1 - number2 
         
        }
    }

}


