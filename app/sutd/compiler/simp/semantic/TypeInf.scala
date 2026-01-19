package sutd.compiler.simp.semantic

import sutd.compiler.simp.syntax.AST.*
import sutd.compiler.simp.monad.Monad.*


object TypeInf {
    import Stmt.*
    import Exp.*
    enum Type {
        case IntTy
        case BoolTy
    }

    enum ExType {
        case MonoType(t:Type)
        case TypeVar(n:String)
    }

    type TypeEnv = Map[Var, Type]
    
    /**
      * Type constraints
      */
    type TypeConstrs = Set[(ExType, ExType)]

    import ExType.*
    /**
      * extract type variable names from a set of type constraints
      *
      * @param tcs type constraints
      * @return a set of names
      */
    def getTVars(tcs:TypeConstrs):Set[String] = tcs.toList.flatMap( p => p match {
        case (TypeVar(n1), TypeVar(n2)) => List(n1,n2)
        case (TypeVar(n1), _) => List(n1)
        case (_, TypeVar(n2)) => List(n2)
        case (_, _) => Nil
    }
    ).toSet

    /**
      * type substitutions, mapping tyvar name to ExType
      */
    enum TypeSubst {
        case Empty // [] 
        case RevComp(s:(String, ExType), psi:TypeSubst) //  psi compose s
    }
    import TypeSubst.*

    /**
     * make a singleton type subsitution
     *
     * @param n - variable name
     * @param t - the type for substitution
     * @return
     */
    def single(n:String, t:ExType):TypeSubst = RevComp((n, t), Empty)

    /**
     * composition of two type substitutions 
     *
     * @param ts1
     * @param ts2
     * @return ts1 o ts2
     */
    def compose(ts1:TypeSubst, ts2:TypeSubst):TypeSubst = ts2 match {
        case Empty => ts1
        case RevComp(x,ts3) => RevComp(x, compose(ts1, ts3))
    }

    trait Substitutable[A] {
        def applySubst(typeSubst:TypeSubst)(a:A):A 
    }

    /**
      * Apply a type substutiton to an extended type 
      *
      * @param tysubst
      * @param type
      * @return either an error or a grounded monotype
      */
    given exTypeSubstitutable:Substitutable[ExType] = new Substitutable[ExType]{
        def applySubst(tysubst:TypeSubst)(ty:ExType):ExType = tysubst match {
            // Lab 2 Task 2.1

            case Empty => ty
            case RevComp((alpha, exTy), Empty) => ty match {
                case TypeVar(a) if a == alpha => exTy
                case TypeVar(b) => ty
                case MonoType(t) => ty
            }

            // psi1 compose psi2
            // this might be hardcoded whoops
            case RevComp((a2, exTy2), psi1) => applySubst(psi1)(
                applySubst(RevComp((a2, exTy2),Empty))(ty)
            )
            // Lab 2 Task 2.1 end
        }
    }

    /**
      * Apply a type substution to a pair of things
      */
    given pairSubstitutable[A,B](using ia:Substitutable[A])(using ib:Substitutable[B]):Substitutable[(A,B)] = new Substitutable[(A,B)]{
        def applySubst(tysubst:TypeSubst)(p:(A,B)): (A,B) = p match {
            case (a,b) => {
                val a1 = ia.applySubst(tysubst)(a)
                val b1 = ib.applySubst(tysubst)(b)
                (a1,b1)
            }
        }
    }

    /**
      * Apply a type substution to a list of things
      */

    given listSubstitutable[A](using i:Substitutable[A]):Substitutable[List[A]] = new Substitutable[List[A]]{
        def applySubst(typeSubst: TypeSubst)(la: List[A]): List[A] = la.map(a => i.applySubst(typeSubst)(a))
    }
    

    import Type.*

    trait Infer[A] {
        def infer(a:A):TypeConstrs
    }

    /**
      * Type inference for SIMP statements
      */

    given infList[A](using infA:Infer[A]):Infer[List[A]] = new Infer[List[A]] {
        def infer(l:List[A]):TypeConstrs = 
            l.foldLeft(Set())( (k, a) => infA.infer(a) union k)
    }

    given infStmt:Infer[Stmt] = new Infer[Stmt] {
        def infer(s:Stmt):TypeConstrs = s match {
            
            case Nop => Set() 
            
            case Assign(x, e) => {
                val n = varname(x)
                val alphax = TypeVar(n)
                inferExp(e) match {
                    case (exTy, k) => k + ((alphax, exTy))
                }
            }
            
            case Ret(x) => Set()
            
            // Lab 2 Task 2.3
            case If(e, s2, s3) => inferExp(e) match {
                case (exTy1, k1) => infList.infer(s2) match {
                    case k2 => infList.infer(s3) match {
                        case k3 => k1 ++ k2 ++ k3 + ((exTy1, MonoType(IntTy)))
                    }
                }
            }

            case While(e, s2) => inferExp(e) match {
                case (exTy1, k1) => infList.infer(s2) match {
                    case k2 => k1 ++ k2 + ((exTy1, MonoType(BoolTy)))
                }
            } 
            // Lab 2 Task 2.3 end
            
        }
    }

    import Const.*
    /**
      * Type inference for SIMP expressions
      *
      * @param e
      * @return
      */
    def inferExp(e:Exp):(ExType, TypeConstrs) = e match {
        
        case ConstExp(IntConst(v)) => (MonoType(IntTy), Set())
        
        case ConstExp(BoolConst(v)) => (MonoType(BoolTy), Set())
        
        case VarExp(v) => {
            val n = varname(v)
            (TypeVar(n), Set())
        }
        
        case ParenExp(e) => inferExp(e)
        
        // Lab 2 Task 2.3
        // case _ => (MonoType(IntTy), Set()) // fixme

        case Plus(e1, e2) => inferExp(e1) match {
            case (exTy1, k1) => inferExp(e2) match {
                case (exTy2, k2) => (MonoType(IntTy), k1 ++ k2 + ((exTy1, MonoType(IntTy)), (exTy2, MonoType(IntTy))))
            }
        }

        case Minus(e1, e2) => inferExp(e1) match {
            case (exTy1, k1) => inferExp(e2) match {
                case (exTy2, k2) => (MonoType(IntTy), k1 ++ k2 + ((exTy1, MonoType(IntTy)), (exTy2, MonoType(IntTy))))
            }
        }

        case Mult(e1, e2) => inferExp(e1) match {
            case (exTy1, k1) => inferExp(e2) match {
                case (exTy2, k2) => (MonoType(IntTy), k1 ++ k2 + ((exTy1, MonoType(IntTy)), (exTy2, MonoType(IntTy))))
            }
        }

        case DEqual(e1, e2) => inferExp(e1) match {
            case (exTy1, k1) => inferExp(e2) match {
                case (exTy2, k2) => (MonoType(BoolTy), k1 ++ k2 + ((exTy1, exTy2)))
            }
        }

        case LThan(e1, e2) => inferExp(e1) match {
            case (exTy1, k1) => inferExp(e2) match {
                case (exTy2, k2) => (MonoType(BoolTy), k1 ++ k2 + ((exTy1, exTy2)))
            }
        }

        // Lab 2 Task 2.3 end        
    } 

    /**
      * unification type class
      */
    trait Unifiable[A] {
        def mgu(a:A):Either[String,TypeSubst]
    }
   
    /**
      * unifying two ExTypes
      */
    given extypesUnifiable:Unifiable[(ExType, ExType)] = new Unifiable[(ExType, ExType)] {
        def mgu(p:(ExType,ExType)):Either[String,TypeSubst] = p match {
            // Lab 2 Task 2.2
            // Lab 2 Task 2.2
            case (exTy1, exTy2) => exTy1 match {
                case MonoType(IntTy) => exTy2 match {
                    case MonoType(IntTy) => Right(Empty) // mgu(int,int)
                    case MonoType(BoolTy) => Left(s"error: unable to unify ${p.toString}") // mgu(int,bool)
                    case TypeVar(alpha) => Right(single(alpha, exTy1)) // mgu(int,alpha)
                }
                case MonoType(BoolTy) => exTy2 match {
                    case MonoType(BoolTy) => Right(Empty) // mgu(bool,bool)
                    case MonoType(IntTy) => Left(s"error: unable to unify ${p.toString}") // mgu(bool,int)
                    case TypeVar(alpha) => Right(single(alpha, exTy1)) // mgu(bool,alpha)
                }
                case TypeVar(alpha) => Right(single(alpha, exTy2)) // mgu(alpha, _)
            }

            // case (exTy1, exTy2) => Left(s"error: unable to unify ${p.toString}") // fixme
            // Lab 2 Task 2.2 end
        }
    }

    /**
      * unifying a set of type constraints (i.e. a set of (ExType, ExType))
      */
    given typeConstrsUnifiable:Unifiable[TypeConstrs] = new Unifiable[TypeConstrs] {
        def mgu(tyconstrs:TypeConstrs) :Either[String,TypeSubst] = {
            listUnifiable.mgu(tyconstrs.toList) 
        } 
    }
    
    given listUnifiable[A](using u:Unifiable[A])(using s:Substitutable[List[A]]):Unifiable[List[A]] = new Unifiable[List[A]] {
        def mgu(l:List[A]):Either[String, TypeSubst] = {
            l match {
                // Lab 2 Task 2.2
                case Nil => Right(Empty)
                case (exTy1::exTys) => exTys match {
                    case Nil => u.mgu(exTy1)
                    case (exTy2::exTyss) => for {
                        psi1 <- u.mgu(exTy1)
                        kp = s.applySubst(psi1)(exTys)
                        psi2 <- mgu(kp)
                    } yield compose(psi2, psi1)
                }
                // Lab 2 Task 2.2 end
            }
        }
    }




    /**
      * grounding a type variable's name given a type substitution 
      *
      * @param varname
      * @param subst
      * @param i
      * @return
      */
    def ground(varname:String, subst:TypeSubst)(using i:Substitutable[ExType]):Either[String, Type] = i.applySubst(subst)(TypeVar(varname)) match {
        case MonoType(t) => Right(t)
        case _ => Left(s"error: type inference failed. ${varname}'s type cannot be grounded ${subst}.")
    }

    /**
      * top level type inference function
      *
      * @param s - list of SIMP Statements
      * @param i - inference statement type class instance
      * @param u - unifiable type class instance
      * @return either an error or a type environment
      */
    def typeInf(s:List[Stmt])(using i:Infer[List[Stmt]])(using st:Substitutable[ExType])(using u:Unifiable[TypeConstrs]):Either[String, TypeEnv] = {
        val typeConstraints = i.infer(s)
        u.mgu(typeConstraints) match {
            case Left(errorMessage) => Left(errorMessage)
            case Right(subst) => {
                val varnames:List[String] = getTVars(typeConstraints).toList                
                def agg(acc:TypeEnv,varname:String):Either[String, TypeEnv] = for {
                    ty <- ground(varname, subst) 
                } yield (acc + (Var(varname) -> ty))
                foldM(agg)(Map():TypeEnv)(varnames)
            }
        }
    }


}