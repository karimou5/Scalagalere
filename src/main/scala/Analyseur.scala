// Analyseur de types pour le lambda calcul simplement typé
object Analyseur:
  // Types du lambda calcul
  enum LambdaType:
    case BasicType(s: String)
    case FunctionType(t1: LambdaType, t2: LambdaType)

  // Termes du lambda calcul
  enum LambdaTerm:
    case Var(varName: String)
    case Abs(varName: String, varType: LambdaType, t: LambdaTerm)
    case App(t1: LambdaTerm, t2: LambdaTerm)

  // Type pour l'environnement de typage
  type TypeEnv = Map[String, LambdaType]

  // Fonction qui vérifie si un terme a le type attendu
  def verifierType(term: LambdaTerm, expectedType: LambdaType): Boolean =
    infererType(term) match
      case Some(inferredType) => inferredType == expectedType
      case None => false

  // Fonction qui infère le type d'un terme
  def infererType(term: LambdaTerm): Option[LambdaType] =
    infererTypeEnv(Map.empty, term)

  // Fonction auxiliaire pour l'inférence avec environnement
  private def infererTypeEnv(env: TypeEnv, term: LambdaTerm): Option[LambdaType] =
    term match
      // Pour une variable, on retourne son type s'il est défini dans l'environnement
      case LambdaTerm.Var(varName) =>
        env.get(varName)

      // Pour une abstraction (λx:T.M), on ajoute x:T à l'environnement
      // et on infère le type du corps M pour déterminer T -> type(M)
      case LambdaTerm.Abs(varName, varType, body) =>
        val newEnv = env + (varName -> varType)
        infererTypeEnv(newEnv, body).map(bodyType =>
          LambdaType.FunctionType(varType, bodyType)
        )

      // Pour une application (M N), on vérifie que M a un type T1 -> T2
      // et que N a bien le type T1, puis on retourne T2
      case LambdaTerm.App(t1, t2) =>
        for
          funType <- infererTypeEnv(env, t1)
          argType <- infererTypeEnv(env, t2)
          resultType <- funType match
            case LambdaType.FunctionType(paramType, bodyType) if paramType == argType => Some(bodyType)
            case _ => None
        yield resultType