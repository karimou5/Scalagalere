enum LambdaTerm:
  case Var(varName: String)
  case Abs(varName: String, varType: LambdaType, t: LambdaTerm)
  case App(t1: LambdaTerm, t2: LambdaTerm)
