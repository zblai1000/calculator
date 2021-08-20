package calculator.controller.view

import scalafx.Includes._
import scalafx.event.ActionEvent
import scalafx.scene.control.{Button,Label}
import scalafx.scene.layout.{AnchorPane}
import java.io.File
import scalafxml.core.macros.sfxml
import scalafx.application.Platform
import scalafx.scene.input.{KeyCode,KeyEvent}
import java.awt.event.ActionListener
import scala.collection.mutable.ArrayBuffer
import calculator.model.UnaryEquation
import calculator.model.BinaryEquation
import calculator.model.Number
import calculator.model.History
import calculator.model.Operator
import scalafx.scene.control.ScrollPane
import scalafx.scene.control.TextArea
import scala.math.BigDecimal
import javafx.scene.text.Font



@sfxml
class CalculatorController(
    private val calculator: AnchorPane, 
	private val one: Button, 
	private val two: Button, 
	private val three: Button, 
	private val four: Button,
    private val five: Button, 
	private val six: Button, 
	private val seven: Button, 
	private val eight: Button, 
	private val nine: Button, 
    private val zero: Button, 
	private val addition: Button, 
	private val subtraction: Button, 
	private val multiplication: Button, 
	private val division: Button,
	private val decimal: Button, 
	private val clear: Button, 
	private val screen: Label, 
	private val operatorScreen: Label, 
	private val scrollPane: ScrollPane){



	//list of numbers as singleton object for user to appeand number(s) together when inputting a value
	//on the calculator 
	private val numbers: List[Integer] = List(Number.zero, Number.one, Number.two, Number.three, Number.four, Number.five, 
									Number.six, Number.seven, Number.eight, Number.nine) 

	//list of operators as single object to be assigned to the operator user selected to execute a calculation
	private val operators: List[String] = List(Operator.addition, Operator.subtraction, 
										Operator.multiplication, Operator.division)

	//to store past calculations executed
	//each calculation executed will be considered as 1 History object which will be stored
	//in this Observable Buffer 
	private var histories: ArrayBuffer[History] = new ArrayBuffer[History]

	//store and joins the numbers selected by users as a string which will then be converted to 
	//a BigDecimal object before passing to an equation to be calculated 
	private var value: String = ""

	//this calculator runs on this format:
	//number1 _ number2 = answer
	//number1 -> is the accumulator of the answer of all the calculation executed from the time of running
	//           the application
	//			Number1 also retrieve the very first value inserted by the user from the time of running the
	//			application 
	//number2 -> retrieve the next value inserted by user when number1 is already holding another value 
	//_ -> operator chosen (addition, subtraction, multiplication, division)
	//answer -> retrieves the answer of the calculation executed and assign the value to number1

	//thus, after the first calculation is executed, it will store the answer in number1 and keeps adding onto
	//number1 for each calculation executed later, where number2 will be used to retrieve new values.
	//for example:
		//number1 + number2 = answer
		//1 + 1 = 2 (answer assign to number1)
		//2 + 1 = 3 (number1 already has the value 2 and number2 will be receiving new input which is 1)
		//3 + 1 = 4 (the process continues)

	//store BigDecimal objects (this calculator will be using 3 of them)
	private var number1: BigDecimal = null
	private var number2: BigDecimal = null
	private var answer: BigDecimal = 0 

	//store operator chosen by user to execute the calculation 
	private var operator: String = ""

	//check if any operator button has been clicked or not 
	private var operationClicked: Boolean = false 

	//check if there are any 
	private var error: Boolean = false

	//controls the contents to be displayed on the screen where the inserted number will be displayed 
	private def display(textToDisplay: String) = {

		//limit display size of the number to 16 characters
		//if over limit will have "..." at the back of the number to indicate that the inserted number 
		//is longer than what is shown on the screen 
		//cut the size of the number to fit the screen 
		if(textToDisplay.size > 16){

			var tempTextToDisplay = textToDisplay.substring(0, textToDisplay.length - (textToDisplay.size - 16))
			screen.text = tempTextToDisplay
		}
		//is the number has lesser than 16 characters, just display the actual number on the screen 
		else{
			screen.text = textToDisplay
		}
	}

	//check for parenthesis in the inserted value and handle it appropriately for calculation 
	private def checkParenthesis(value: String): String = {

		//assinged modified version of value into tempValue 
		var tempValue: String = ""
		
		//if the value is in the parenthesis:
			//example: (123)
		//remove the parenthesis fron the front and back of the value
			//example: result in -> 123
		if(value.take(1).equals("(") && value.reverse.take(1).equals(")")){

			tempValue = value.substring(1, value.length() -1)
			error = false
		}
		//if the value contains "(" but not in the beginning 
			//example: 2(23		
		//this will mean 2 x 23, which will be compiled into a number by executing the calculation 
		//thus this will result in 46
		//for example if the user input: 1(2 + 2 as number1, operator and number2 respectively
		// 1(2 will be compiled to 2 (1x2) as number1, thus the resulting equation to be passed is:
		// 2 + 2
		else if(value.contains("(") && !value.take(1).equals("(") && !value.reverse.take(1).equals("(")
						&& !value.contains(")")){

			//retrieve the index of "(" in the value
			var frontParenthesisPosition: Int = value.indexOf("(")
			//retrieve the characters before "(" in the value
			var frontValue: String = ""
			//retrieve the characters after "(" in the value
			var backValue: String = ""

			
			for(i <- 0 until value.size){

				//when reached the position of "("
				if(i == frontParenthesisPosition){

					//loop through the value starting from the character after "("
					for(j <- frontParenthesisPosition + 1 until value.size){

						//save the characters into backValue
						backValue = backValue + value(j)
					}	
				}
				//when has not reached the position of "("
				else if (i < frontParenthesisPosition){

					//save the characters into frontValue
					frontValue = frontValue + value(i)
				}
			}

			//compile frontValue and backValue into 1 value via multiplication
			tempValue = runBinaryCalculation(BigDecimal(frontValue), BigDecimal(backValue), Operator.multiplication).toString
			//no error as this does not violate any mathematical rules 
			error = false
			
		}
		//if the value front and end does not have a parenthesis:
			//example: 123		OR		1(23 		OR 		12)3
		else if(!value.take(1).equals("(") && !value.reverse.take(1).equals(")")){

			//check if the value contains parenthesis or multiplication operator or division operator 
				//example: 1*23		OR 1/23
				//note: although 1/23 is a valid number mathematically, the design of this calculator will
				//execute the calculation when user select "1" before inserting the value of "23"
				//thus, the value should not contain "/", but will contain "1" and "23" on 2 seperate execution
				//of the calculation respectively 
			if (value.contains("(") || value.contains(")")|| value.contains("x")|| value.contains("÷")) {

				//set the display to show math error 
				//set error to true which will restrict the use of all the buttons aside "Clear"
				display("Math ERROR")
				tempValue = "0"
				error = true
			}
			//check if the value does not contain "(" at the front but contain ")" at the back
			//example: 123)
			else if (!value.take(1).equals("(") && value.reverse.take(1).equals(")")){

				//set the display to show math error 
				//set error to true which will restrict the use of all the buttons aside "Clear"
				display("Math ERROR")
				tempValue = "0"
				error = true
			}
			//if none of the conditions mentioned above fit, then proceed without error 
		
			else{

				//assign value to tempValue without the need to modify the contents of the value 
				tempValue = value
				error = false
			}
		}
		//if there is "(" at the front of the value and no ")" at the back of the value
			//example: (123		(1)23		12(3
		else if(value.take(1).equals("(") && !value.reverse.take(1).equals(")")){

			//remove "(" at the front of the value and assign to tempValue
			tempValue = value.substring(1)
			
			//if value does not contain parenthesis 
			if(!tempValue.contains("(") && !tempValue.contains(")")){
				
				//proceed without error 
				error = false
			}
			else{

				//set error to true which will restrict the use of all the buttons aside "Clear"
				display("Math ERROR")
				tempValue = "0"
				error = true
			}
		}
		//if there is no "(" at the front of the value but there is "(" at the end of the value
			//example: 123)
		//note: possibility of "(" or ")" appearing within the value has already been check above,
			//example: 12(3		12)3
		else if(!value.take(1).equals("(") && value.reverse.take(1).equals(")")){

			//set error to true which will restrict the use of all the buttons aside "Clear"
			display("Math ERROR")
			tempValue = "0"
			error = true
		}
		//if the value does not contains parenthesis 
			//example: 123
		//do not need to edit the value
		//just assign the value as it is to tempValue
		else if (!value.contains("(") || !value.contains(")")){

			tempValue = value 
			error = false
		}
		//if none of the conditions mentioned above match
		//it is a mathematical error 
		else
		{

			display("Math ERROR")
			tempValue = "0"
			error = true
		}
		//assign to number1 and number2 by executing:
			//number1 = BigDecimal(checkParenthesis(value))
			//OR
			//number2 = BigDecimal(checkParenthesis(value))
		//at the appropriate places 
		return tempValue

	}

	//check if there is any errors
	//only execute calculation and add the calculation done on the history screen 
	//if there is no error 
	private def handleCalculation() = {

		if (!error){

			calculate()
			writeOnHistoryScreen(retrieveResultsFromHistories())
	
		}
	}

	//this method is called when user click on an operator button
		//operators: + , - , x , ÷
	private def handleClickOperations() = {
		
		//when user insert the first value since the starting of the application 
		//where the value is not empty
		//check if there is already + or - state assigned to value, if there is, then do not assign value to number1
		if(number1 == null && answer == 0 && !value.equals("") && !value.equals("(")
			&& !value.contains("(-") && !value.contains("(+")){

			//assign to number1
			number1 = BigDecimal(checkParenthesis(value))
			//reset value to empty string which will be used to store the next number inserted 
			value = ""
	
		}

		//when user insert the second value since the stating of the application or consecutive values
		//after the first value inserted where the value is not empty 
		//there are 2 scenarios here where number2 is assigned a value:
			//scenario 1: user insert 1 + 2, where 1 is number1 and 2 is number2
			//scenario 2: user does a unary calculation on the 1st caclulation which will leave number2 empty
				//example: +2
		//check if there is already + or - state assigned to value, if there is, then do not assign value to number2
		else if(number2 == null && number1 != null && !value.equals("") && !value.equals("(")
				&& !value.contains("(-") && !value.contains("(+")){

				//assign to number2 
				number2 = BigDecimal(checkParenthesis(value))
				//reset value to empty string which will be used to store the next number inserted 
				value = ""

			//if number1 and number2 are not null, check for error before running calculation
			if(number1 != null && number2 != null){
	
				//reset value to empty string which will be used to store the next number inserted 
				value = ""
				//error checking will be done here, if no error will execute calculation 
				handleCalculation()
			}
		
		}
		//if values inserted after the unary (first) calculation have parenthesis 
		//check if there is already + or - state assigned to value, if there is, then do not assign value to number2
		else if(number2 == null && number1 != null && !value.equals("") && value.contains("(") && !value.equals("(")
				&& !value.contains("(-") && !value.contains("(+")){

			//check for parenthesis and assigned filtered value into number2
			//if the parenthesis results in error, will assign 0 to number2 and all buttons on the calculator will 
			//be disabled aside the "Clear button"
			number2 = BigDecimal(checkParenthesis(value))
			//reset value to empty string which will be used to store the next number inserted 
			value = ""

			//if number1 and number2 is not null, check for error before running calculation
			if(number1 != null && number2 != null){
	
				//reset value to empty string which will be used to store the next number inserted 
				value = ""
				//error checking will be done here, if no error will execute calculation 
				handleCalculation()
			}
		}
		//if number1 is null and number2 is null but answer is not 0, assign answer to number1
		//as number1 acts as a cummulative number for the consecutive calculations
		else if(answer != 0 && number1 == null && number2 == null){

			//reset value to empty string which will be used to store the next number inserted 
			value = ""
			//assign answer to number1
			number1 = answer
		}
	}

	//execute calculations
	//there are 2 types of execution:
		//unary equation (only used when number2 is null)
		//binary equation
	private def calculate() = {

		//when number2 is null and an operator is chosen, runs unary calculation 
		if(number2 == null && !operator.equals("")){

			//if operator chosen is + or -, runs calculation as +num or -num does not 
			//violate any mathematical law 
			if(operator.equals(Operator.addition) || operator.equals(Operator.subtraction)){

				//run unary calculation 
				answer = runUnaryCalculation(number1, operator)
				//display answer on the screen
				display(answer.toString)
				//add calculation to History
				histories += new History(0, number1, operator, answer)
			}
			//if operator chosen is x or ÷, do not run calculation and display "syntax error"
			//as *num or ÷num violates the mathematical law and results in a syntax error 
			else if(operator.equals(Operator.multiplication) || operator.equals(Operator.division)){

				answer = 0
				display("Syntax ERROR")
				error = true
			}	
		}
		
		//if number1, number2 and value is not null and operator is selected 
		if (number2 != null && number1 != null && !value.equals("") && !operator.equals("")){

			//if operator selected is x and number2 is 0, assign 0 to answer as:
				//AnyNumber x 0 = 0
			if(operator.equals("x") && number2 == 0){

				answer = 0
			}
			//if operator selected is ÷ and number2 is 0, display "math error" as:
				//AnyNumber ÷ 0 results in a math error
			else if(operator.equals("÷") && number2 == 0){

				answer = 0
				display("Math ERROR")
				error = true
			}
			//if none of the conditions mentioned above meets, then execute binary calculation
			else{
				
				//runs binary calculation
				answer = runBinaryCalculation(number1, number2, operator)
				//display answer on screen
				display(answer.toString)
				//add calculation to History 
				histories += new History(number1, number2, operator, answer)
			}
		}
		//if number2 is null and operator is not selected but number1 is not null
		//this event occur when user input a number and then click "="
		else if (number2 == null && number1 != null && operator.equals("")){

			//display the value inserted by user on screen
			display(value)
			//set answer to number1, where the next value inserted will result in a binary calculation
			//as number1 is no longer null
			answer = number1
		}

		//reset value to empty which will be used to retrieve the next number inserted by user
		value = ""
		//assign number1 to answer 
		number1 = answer
		//set number2 to null which will used to retrieve the next number inserted by user through value variable 
		number2 = null
		//remove the operator selected by user
		operator = ""
		//set operator screen to display "=", which is used to indicate the the calculation has been executed 
		operatorScreen.text = "="
	}

	//handle event when user click on "="
	def handleClickEqual(action: ActionEvent) = {

		//only run the logic in this method when there no mathematical law has been violated 
		if(!error){

			//if number1 and number2 are null and operator is not selected but value is not empty
			//this occurs when user input the 1st value since starting the calculator and then hit "="
			if(number2 == null && number1 == null && operator.equals("") && !value.equals("")){

				//assign value to number1 after checking for parenthesis 
				//if the parenthesis is placed in a way that results in an error, number1 will be set to 0 
				//and the error will be displayed on the screen 
				number1 = BigDecimal(checkParenthesis(value))
				//error checking will be done here, if no error will execute calculation 
				handleCalculation()
			}
			//if number1 and number2 are null and operator is selected but value is not empty
			//this occurs when user input for example: +1, -1
			else if(number2 == null && number1 == null && !operator.equals("") && !value.equals("")){

				//assign value to number1 after checking for parenthesis 
				//if the parenthesis is placed in a way that results in an error, number1 will be set to 0 
				//and the error will be displayed on the screen 
				number1 = BigDecimal(checkParenthesis(value))
				//error checking will be done here, if no error will execute calculation 
				handleCalculation()
			}
			//if only number2 is null and an operator is selected
			else if(number2 == null && number1 != null && !operator.equals("") && !value.equals("")){

				//assign value to number2 after checking for parenthesis 
				//if the parenthesis is placed in a way that results in an error, number2 will be set to 0 
				//and the error will be displayed on the screen 
				number2 = BigDecimal(checkParenthesis(value))
				//error checking will be done here, if no error will execute calculation 
				handleCalculation()
			}
		}		
	}
	
	//return resulting value of a binary calculation
	private def runBinaryCalculation(num1: BigDecimal, num2: BigDecimal, operatorUsed: String): BigDecimal = {

		return new BinaryEquation(num1, num2, operatorUsed).executeEquation
	}

	//return resulting value of a unary calculation
	private def runUnaryCalculation(num: BigDecimal, operatorUsed: String): BigDecimal = {

		return new UnaryEquation(num, operatorUsed).executeEquation
		
	}

	//handle event when user click on decimal button 
	def handleClickDecimal(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a decimal
			//however, user do not need to select an operator if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//check if there is already a decimal in the value
			//if there is no decimal in the value -> allow the insert of a decimal
			//if there is decimal in the value -> do not allow the insert of a decimal
			//this is because there cannot be more than 1 decimal in a number 
			if(!value.contains(".")){

				value = value + "."
				display(value)
			}	
		}
	}

	//assign positive or negative state to value on the appropriate input 
	private def checkValueState() = {

		//if both number1 and number2 are null, add "+" or "-" into value 
		//this will result in a positive or negative number for the first value inserted 
		//upon starting the application 
		if(!operator.equals("") && value.equals("") && number1 == null && number2 == null){

			if(operator.equals(Operator.addition) || operator.equals(Operator.subtraction)){

				value = value + operator
			}
			
		}
	}

	//handle event when user click on number one button 
	def handleClickOne(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add one into value
			addToValue(Number.one)
			//update the screen to reflect the new number 
			display(value)
		}

		
	}
	
	//handle event when user click on number two button 
	def handleClickTwo(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add two into value
			addToValue(Number.two)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number three button 
	def handleClickThree(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add three into value
			addToValue(Number.three)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number four button 
	def handleClickFour(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add four into value
			addToValue(Number.four)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number five button 
	def handleClickFive(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add five into value
			addToValue(Number.five)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number six button 
	def handleClickSix(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add six into value
			addToValue(Number.six)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number seven button 
	def handleClickSeven(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add seven into value
			addToValue(Number.seven)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number eight button 
	def handleClickEight(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add eight into value
			addToValue(Number.eight)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number nine button 
	def handleClickNine(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add nine into value
			addToValue(Number.nine)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on number zero button 
	def handleClickZero(action: ActionEvent) = {

		//only execute method when there is no error
		//and
		//either operator is selected or number1 is null 
			//this forces user to select an operator before inserting a number
			//however, user do not need to select a number if number1 is null, 
			//which indicates no calculation has been done before since the starting 
			//of the application 
		if(!error && (!operator.equals("") || number1 == null)){

			//assign positive or negative state to value on the appropriate input 
			checkValueState()
			//add zero into value
			addToValue(Number.zero)
			//update the screen to reflect the new number 
			display(value)
		}
	}

	//handle event when user click on additional operator button 
	def handleClickAddition(action: ActionEvent) = {

		//only execute method when there is no error
		if(!error){

			//run this method to determine if it is necessary to execute calculation or not
			//if not necessary to execute calculation, assign value to either number1 or number2
			//depending on the condition. All these are done in handleClickOperations(). 
			handleClickOperations()
			//only allow addition operator to be selected when value is empty
			//value is empty shows that the value is available for new intake of number
			if(value.equals("")){

				//set operator to addition
				operator = Operator.addition
				//show addition operator on operator screen 
				operatorScreen.text = operator
				
			}
			//if value is "(" 
			//allow "+" to be added into the value, which will result in: "(+"
			//which is valid 
			//note that since the 
			else if(value.equals("(")){

				//add addition into value
				addToValue(Operator.addition)
				//update the screen to reflect the new state of the number 
				display(value)
			}
		}
	}

	//handle event when user click on subtraction operator button 
	def handleClickSubtraction(action: ActionEvent) = {

		//only execute method when there is no error
		if (!error){

			//run this method to determine if it is necessary to execute calculation or not
			//if not necessary to execute calculation, assign value to either number1 or number2
			//depending on the condition. All these are done in handleClickOperations(). 
			handleClickOperations()
			//only allow subtraction operator to be selected when value is empty
			//value is empty shows that the value is available for new intake of number
			if(value.equals("") && number1 != null){

				//set operator to subtraction
				operator = Operator.subtraction
				//show subtraction operator on operator screen 
				operatorScreen.text = operator
				
			}
			//if value is "(" 
			//allow "-" to be added into the value, which will result in a negative value
			else if(value.equals("") && number1 == null){

				//set operator to addition
				//this is to allow unary calculation: 0 + (-number) = - number 
				operator = Operator.addition
				//add subtraction into value
				addToValue(Operator.subtraction)
				//update the screen to reflect the new state of the number 
				display(value)
			}
			//if value is "(" 
			//allow "-" to be added into the value, which will result in: "(-"
			//which is valid 
			//note that since the 
			else if(value.equals("(")){

				//add subtraction into value
				addToValue(Operator.subtraction)
				//update the screen to reflect the new state of the number 
				display(value)
			}
		}
	}

	//handle event when user click on multiplication operator button 
	def handleClickMultiplication(action: ActionEvent) = {

		//only execute method when there is no error
		if(!error){

			//run this method to determine if it is necessary to execute calculation or not
			//if not necessary to execute calculation, assign value to either number1 or number2
			//depending on the condition. All these are done in handleClickOperations(). 
			handleClickOperations()
			//only allow multiplication operator to be selected when value is empty
			//value is empty shows that the value is available for new intake of number
			if(value.equals("")){

				//set operator to multiplication
				operator = Operator.multiplication
				//show multiplication operator on operator screen 
				operatorScreen.text = operator
			}
		}
	}

	//handle event when user click on division operator button 
	def handleClickDivision(action: ActionEvent) = {

		//only execute method when there is no error
		if(!error){

			//run this method to determine if it is necessary to execute calculation or not
			//if not necessary to execute calculation, assign value to either number1 or number2
			//depending on the condition. All these are done in handleClickOperations().
			handleClickOperations()
			//only allow division operator to be selected when value is empty
			//value is empty shows that the value is available for new intake of number
			if(value.equals("")){

				//set operator to division
				operator = Operator.division
				//show division operator on operator screen 
				operatorScreen.text = operator
			}
		}
	}

	//handle event when user click on left parenthesis button 
	def handleClickLeftParenthesis(action: ActionEvent) = {

		//if there is no error and
		//operator is selected or number1 is null
		if(!error && (!operator.equals("") || number1 == null)){

			//if "(" is not in value 
			if(!value.contains("(")){

				//add "(" into value
				value = value + "("
				//update the screen to reflect the new parenthesis added 
				display(value)
			}
		}
	}

	//handle event when user click on right parenthesis button 
	def handleClickRightParenthesis(action: ActionEvent) = {

		//if there is no error and
		//operator is selected or number1 is null
		if(!error && (!operator.equals("") || number1 == null)){

			//if ")" is not in value 
			if(!value.contains(")")){

				//add ")" into value
				value = value + ")"
				//update the screen to reflect the new parenthesis added 
				display(value)
			}
		}
	}

	//adds Integer (numbers) and String (operators) into value variable which is
	//used to assign number1 and number2 which are the variables that holds the values
	//used to execute the calculation
	private def addToValue(any: AnyRef) = {

		value = value + any
	}

	//retrieve calculations executed in the past from History array 
	//this only includes calculations from the starting of the application every time
	//calculations are not saved after the application has stop 
	private def retrieveResultsFromHistories(): String = {

		//to store equations in the form of a string 
		var tempHistories: String = ""

		//loop through the History array
		for(i <- 0 until histories.size){

			//store all the equations into tempHistories 
			tempHistories = tempHistories + histories(i).showHistory()
		}
		//return a string of all the equations added together 
		return tempHistories
	}

	//write on the history screen 
	private def writeOnHistoryScreen(recordsToDisplay: String) = {

		//create a text area for the past calculations to be displayed on
		var historyTextArea: TextArea = new TextArea(recordsToDisplay)
		historyTextArea.setStyle("-fx-border-color: #FFFFFF;")
		historyTextArea.minWidth  = 210
		historyTextArea.minHeight = 400
		historyTextArea.setFont(new Font(15))

		//do not allow user to edit the TextArea
		historyTextArea.editable = false

		//place TextArea on the scrollPane to allow user to scroll through the 
		//calculations done before horizonally and vertically 
		//this allows user to have full access to all the calculations no matter the number
		//of calculations done
		//this also allows user to view the full value they keyed in which they might not
		//have seen due to the limitation of the screen display size
		scrollPane.content = historyTextArea
		
	}

	//handle event when user click on Clear button 
	def handleClickClear(action: ActionEvent) = {

		//reset number1, number2 and value to be like during the starting of the application
		//when number1 is still null. 
		number1 = null
		number2 = null
		value = ""

		//clear display to reflect that no values has been inserted yet 
		display("")

		//clear operator screen to reflect that no operators has been chosen yet 
		operatorScreen.text = ""

		//reset operator as no operator is selected by default
		operator = ""

		//reset answer to 0 as no calculations has been executed yet
		answer = 0

		//all errors will be ignored upon resetting, thus enabling all functionalies 
		//of the calculator again (if user made an error before resetting)
		error = false
		
	}

}