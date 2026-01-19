file:///C:/Users/chery/Documents/compiler/project/math-chat/app/controllers/CalculatorController.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 640
uri: file:///C:/Users/chery/Documents/compiler/project/math-chat/app/controllers/CalculatorController.scala
text:
```scala
package controllers

import javax.inject._
import play.api._
import play.api.mvc._

import controllers.CalculatorService

@Singleton
class CalculatorController @Inject()(
    val controllerComponents:ControllerComponents
    calculatorService:CalculatorService
) extends BaseController {

    case class CalculatorRequest(source:String)
    implicit val reqReads:Reads[CalculatorRequest] = Json.reads[CalculatorRequest]

    def calculate() = Action(parse.json) { request => 
        val input = request.body.validate[CalculatorRequest]

        input.fold(
            errors => BadRequest(Json.obj("status"->"error", @@))
        )
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