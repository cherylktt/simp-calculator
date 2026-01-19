package services

import javax.inject._
import scala.io.Source
import sutd.compiler.simp.syntax.Lexer.{given, *}
import sutd.compiler.simp.syntax.Parser.*
import sutd.compiler.simp.syntax.Parsec.*
import sutd.compiler.simp.syntax.Parsec
import sutd.compiler.simp.syntax.Parsec.Progress
import sutd.compiler.simp.syntax.AST.*
import sutd.compiler.simp.semantic.TypeInf.{given, *}
import sutd.compiler.simp.interpreter.SimpInt.*
import sutd.compiler.simp.ir.MMUpDown.*
import sutd.compiler.simp.ir.Util.StateInfo
import sutd.compiler.simp.backend.JVM.*
import sutd.compiler.simp.monad.Monad.*
import sutd.compiler.simp.monad.StateT.*

@Singleton
class CalculatorService {

    import Progress.*
    import Result.*

    def compile(args:String):Either[String, Array[Byte]] = {

        Parsec.run(lex)(LEnv(args.toList, 1, 1)) match {
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
                                            case Right(bytes) => Right(bytes)
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