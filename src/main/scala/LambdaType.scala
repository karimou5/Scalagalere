enum LambdaType:
  case BasicType(s: String)
  case FunctionType(t1: LambdaType, t2: LambdaType)
