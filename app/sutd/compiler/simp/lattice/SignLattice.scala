package sutd.compiler.simp.lattice

import sutd.compiler.simp.lattice.CompleteLattice.{given, *} 

object SignLattice {
    import CompleteLattice.* 
    enum SignAbsVal {
        case Bot    // _|_
        case Minus  // -
        case Plus   // + 
        case Top    // T
        case Zero   // 0
    }

    import SignAbsVal.*
    // Cohort Problem Exercise 2
    given signLattice:CompleteLattice[SignAbsVal] = new CompleteLattice[SignAbsVal] {
        def sqSubSetEq(a: SignAbsVal, b: SignAbsVal): Option[Boolean] = (a,b) match {
            case (x, y) if x == y => Some(true)
            case (Bot, Zero) => Some(true)
            case (Bot, Plus) => Some(true)
            case (Bot, Minus) => Some(true)
            case (Zero, Top) => Some(true)
            case (Plus, Top) => Some(true)
            case (Minus, Top) => Some(true)
            case (_,_) => Some(false)
        }
        def lub(a:SignAbsVal, b:SignAbsVal):SignAbsVal = (a,b) match {
            case (x, y) if x == y => x
            case (Bot, x) => x
            case (x, Bot) => x
            case (_,_) => Top
        }
    }
}