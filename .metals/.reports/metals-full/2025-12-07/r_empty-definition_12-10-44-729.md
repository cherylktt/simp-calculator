error id: file:///C:/Users/chery/Documents/compiler/project/math-chat/app/services/CalculatorService.scala:`<none>`.
file:///C:/Users/chery/Documents/compiler/project/math-chat/app/services/CalculatorService.scala
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -controllers/simp/syntax/Lexer.controllers.simp.syntax.Parsec.
	 -controllers/simp/syntax/Parser.controllers.simp.syntax.Parsec.
	 -controllers/simp/syntax/Parsec.controllers.simp.syntax.Parsec.
	 -controllers/simp/syntax/AST.controllers.simp.syntax.Parsec.
	 -controllers/simp/semantic/TypeInf.controllers.simp.syntax.Parsec.
	 -controllers/simp/interpreter/SimpInt.controllers.simp.syntax.Parsec.
	 -controllers/simp/ir/MMUpDown.controllers.simp.syntax.Parsec.
	 -controllers/simp/backend/JVM.controllers.simp.syntax.Parsec.
	 -controllers/simp/monad/Monad.controllers.simp.syntax.Parsec.
	 -controllers/simp/monad/StateT.controllers.simp.syntax.Parsec.
	 -controllers/simp/syntax/Parsec.
	 -scala/Predef.controllers.simp.syntax.Parsec.
offset: 251
uri: file:///C:/Users/chery/Documents/compiler/project/math-chat/app/services/CalculatorService.scala
text:
```scala
package services

import scala.io.Source
import controllers.simp.syntax.Lexer.{given, *}
import controllers.simp.syntax.Parser.*
import controllers.simp.syntax.Parsec.*
import controllers.simp.syntax.Parsec
import controllers.simp.syntax.Parsec@@.Progress
import controllers.simp.syntax.AST.*
import controllers.simp.semantic.TypeInf.{given, *}
import controllers.simp.interpreter.SimpInt.*
import controllers.simp.ir.MMUpDown.*
import controllers.simp.ir.Util.StateInfo
import controllers.simp.backend.JVM.*
import controllers.simp.monad.Monad.*
import controllers.simp.monad.StateT.*

object Calculator {

    private def bytesToHex(bytes: Array[Byte]):String = bytes.map("%02X".format(_)).grouped(16).map(_.mkString(" ")).mkString("\n")

    import Progress.*
    import Result.*
    def compile(args:String):Either[String, String] = {

        Parsec.run(lex)(LEnv(lines.toList, 1, 1)) match {
            case Consumed(Ok((toks, lenv))) if eof(lenv) => {
                Parsec.run(parse)(PEnv(toks)) match {
                    case Consumed(Ok((stmts, penv))) if done(penv) => {
                        typeInf(stmts) match {
                            case Left(errorMessage) => {
                                Left(errorMessage)
                            }
                            case Right(typeenv) => {
                                cogen(stmts).run(StateInfo(1,"var", 1)) match {
                                    case Identity((st_, instrs)) => {
                                        genTargetCode(instrs) match {
                                            case Right(bytes) => Right(bytesToHex(bytes))
                                            case Left(err) => Left(s"JVM generation error: $err")
                                        }
                                    }
                                }

                            }
                        }
                    }
                    case Consumed(Ok((stmts, penv)))  => {
                        Left(s"Parser terminated prematurely at ${penv._1}")
                    }
                    case Consumed(Failed(msg)) => 
                        Left(msg)
                    case Empty(Ok((stmts, penv))) if done(penv) => {
                        Left("An empty file is given.")
                    }
                    case Empty(Ok((stmts, penv)))  => {
                        Left(s"Parser terminated prematurely at ${penv._1}")
                    }
                    case Empty(Failed(msg)) => 
                        Left(msg)
                }
            }
            case Consumed(Ok((toks, lenv))) => {
                Left(s"Lexer terminated prematurely at ${lenv._1.mkString("")}")
            }
            case Consumed(Failed(msg)) => Left(msg)
            case Empty(Ok((toks, lenv))) if eof(lenv) => {
                Left("An empty file is given.")
            }
            case Empty(Ok((toks, lenv))) => {
                Left(s"Lexer terminated prematurely at ${lenv._1.mkString("")}")
            }
            case Empty(Failed(msg)) => Left(msg)
        }
            
    }
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.