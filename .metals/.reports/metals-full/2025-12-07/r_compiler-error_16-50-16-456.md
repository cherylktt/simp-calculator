file:///C:/Users/chery/Documents/compiler/project/math-chat/app/controllers/CalculatorController.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 833
uri: file:///C:/Users/chery/Documents/compiler/project/math-chat/app/controllers/CalculatorController.scala
text:
```scala
package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import java.util.Base64

import services.CalculatorService

@Singleton
class CalculatorController @Inject()(
    val controllerComponents:ControllerComponents,
    calculatorService:CalculatorService
) extends BaseController {

    def calculator() = Action { implicit request: Request[AnyContent] => 
        Ok(views.html.calculator())
    }

    def calculate() = Action(parse.text) { implicit request: Request[String] =>
        val input = request.body

        if (input.trim.isEmpty) {
            BadRequest(Json.obj(
                "status" -> "error",
                "message" -> "Input cannot be empty"
            ))
        } else {

            val fibPattern = "fibonacci\\((@@))"


            calculatorService.compile(input) match {
                case Right(bytes) => {
                    val base64String = Base64.getEncoder.encodeToString(bytes)
                    Ok(Json.obj(
                        "status" -> "success",
                        "bytecode" -> base64String
                    ))
                }
                case Left(errMsg) => BadRequest(Json.obj(
                    "status" -> "error",
                    "message" -> errMsg
                ))
            }
        }
    }

}


```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1