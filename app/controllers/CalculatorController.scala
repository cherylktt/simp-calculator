package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import java.util.Base64
import scala.io.Source

import services.CalculatorService

@Singleton
class CalculatorController @Inject()(
    val controllerComponents:ControllerComponents,
    calculatorService:CalculatorService,
    environment:Environment
) extends BaseController {

    def calculator() = Action { implicit request: Request[AnyContent] => 
        Ok(views.html.calculator())
    }

    def calculate() = Action(parse.text) { implicit request: Request[String] =>
        val originalInput = request.body

        if (originalInput.trim.isEmpty) {
            BadRequest(Json.obj(
                "status" -> "error",
                "message" -> "Input cannot be empty"
            ))
        } else {

            val algoPattern = "(\\w+)\\((\\d+)\\)".r

            def isExpression(input:String): Boolean = {
                !input.contains(";") &&
                !input.contains("return") &&
                !input.contains("=")
            }

            val inputEither: Either[String, String] = originalInput match {

                // existing algos like fibnoacci
                case algoPattern(name, number) =>
                    val filePath = s"simp/$name.simp"
                    environment.resourceAsStream(filePath) match {
                        case Some(stream) =>
                            val fileContent = Source.fromInputStream(stream).mkString
                            stream.close()
                            Right(s"x = $number;\n" + fileContent)
                        case None => Left(s"Algorithm '$name' not found.")
                    }

                // for input like 3+5 or 4-6
                case input if isExpression(input) => Right(s"res = $input; return res;")
                
                // simp format
                case _ => Right(originalInput)
            }

            inputEither match {
                case Left(fileError) => BadRequest(Json.obj(
                    "status" -> "error",
                    "message" -> fileError
                ))

                case Right(code) =>
                    calculatorService.compile(code) match {
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

}

