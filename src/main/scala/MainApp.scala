package calculator
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.Includes._
import scalafxml.core.{NoDependencyResolver, FXMLView, FXMLLoader}
import javafx.{scene => jfxs}
import scalafx.stage.{Stage, Modality}
import scalafx.event.ActionEvent
import scalafx.scene.image.Image
import calculator.controller.view.RootLayoutController
import calculator.controller.view.CalculatorController
import scalafx.stage.StageStyle

object MainApp extends JFXApp {


    
    val rootResource = getClass.getResourceAsStream("view/RootLayout.fxml")
    val loader = new FXMLLoader(null, NoDependencyResolver)
    // Load root layout from fxml file.
    loader.load(rootResource);
    val roots = loader.getRoot[jfxs.layout.BorderPane]

    stage = new PrimaryStage {
        title = "Calculator"
        scene = new Scene(550,426) {
          root = roots
          stylesheets += getClass.getResource("view/style.css").toString()
        }
        icons += new Image(getClass.getResourceAsStream("/images/icon.png"))
    }
  

    def displayCalculator() = {

      val resource = getClass.getResourceAsStream("view/Calculator.fxml")
      val loader = new FXMLLoader(null, NoDependencyResolver)
      loader.load(resource);
      val roots = loader.getRoot[jfxs.layout.AnchorPane]
      MainApp.roots.setCenter(roots)
    }
    stage.setResizable(false)
    displayCalculator()

}