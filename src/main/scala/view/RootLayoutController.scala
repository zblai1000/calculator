package calculator.controller.view
import scalafxml.core.macros.sfxml
import calculator.MainApp
import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType


@sfxml
class RootLayoutController(){

    //exit application 
    def handleClose(){

        Platform.exit

    }

    //display information about this application 
    def handleAbout(){

        val alert = new Alert(Alert.AlertType.Information){
          initOwner(MainApp.stage)
          title       = "About"
          headerText  = "Calculator App v 1.0"
          contentText = "Made by Lai Zemin Bryan"
        }.showAndWait()
    }
}
