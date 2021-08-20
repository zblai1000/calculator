package calculator.model

import scala.math.BigDecimal

//Binary equation is an equation that has 2 values 
class BinaryEquation(num1: BigDecimal, num2: BigDecimal, operationSelected: String) extends Equation{

    var number1 = num1
    var number2 = num2
    var operation = operationSelected

    //return an answer based on the operator passed into the equation 
    override def executeEquation(): BigDecimal = {

        operation match {

            case "+" => return number1 + number2
            case "-" => return number1 - number2
            case "x" => return number1 * number2
            case "÷" => return number1 / number2
        }
    }

}