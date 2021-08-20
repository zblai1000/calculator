package calculator.model

import scala.math.BigDecimal

//object to store an equation that has been calculated
class History(num1: BigDecimal, num2: BigDecimal, operationSelected: String, answer: BigDecimal) {

    //store the equation in string 
    private var displayHistory: String = ""

    //display the equation in the proper form and store it in a string to be returned 
    def showHistory(): String = {

        //if number2 is a negative value, the put a parenthesis around number2 
        if(num2.toString.contains("-")){

            //example:              1                       +                       (-1)         =          2
            displayHistory = num1.toString + " " + operationSelected + " (" + num2.toString + ") = " + answer.toString + "\n"
        }
        //if number2 is not a negative value, display the equation without use of parenthesis 
        else{

            //example:              1                       +                        1         =               2
            displayHistory = num1.toString + " " + operationSelected + " " + num2.toString + " = " + answer.toString + "\n"
        }
     
        return displayHistory
    }
}

