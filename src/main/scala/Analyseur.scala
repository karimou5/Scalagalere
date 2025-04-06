// Analyseur de types pour le lambda calcul simplement typé
object Analyseur:
  // Types du lambda calcul
  // BasicType("Int") représente le type entier
  // BasicType("Bool") représente le type booléen
  // FunctionType(t1, t2) représente le type fonction de t1 à t2
  enum LambdaType:
    case BasicType(s: String)
    case FunctionType(t1: LambdaType, t2: LambdaType)
    
    override def toString(): String = this match
      case BasicType(s) => s
      case FunctionType(t1, t2) => s"(${t1} → ${t2})"

  // Termes du lambda calcul
  // Var("x") représente une variable
  // Abs("x", intType, Var("x")) représente l'abstraction λx:Int.x
  // App(abstraction, Var("y")) représente l'application de l'abstraction à y
  enum LambdaTerm:
    case Var(varName: String)
    case Abs(varName: String, varType: LambdaType, t: LambdaTerm)
    case App(t1: LambdaTerm, t2: LambdaTerm)
    
    override def toString(): String = this match
      case Var(name) => name
      case Abs(name, tpe, body) => s"λ$name:$tpe.$body"
      case App(t1, t2) => s"($t1 $t2)"

  // Environnement qui représente le contexte de typage
  type Environnement = Map[String, LambdaType]

  // Première fonction: vérifier le type d'un terme
  def verifierType(terme: LambdaTerm, typeAttendu: LambdaType, environnement: Environnement = Map()): Boolean =
    terme match
      case LambdaTerm.Var(nomVariable) =>
        environnement.get(nomVariable) match
          case Some(typeVariable) => typeVariable == typeAttendu
          case None => false

      case LambdaTerm.Abs(nomParametre, typeParametre, corps) =>
        typeAttendu match
          case LambdaType.FunctionType(typeEntree, typeSortie) if typeEntree == typeParametre =>
            verifierType(corps, typeSortie, environnement + (nomParametre -> typeParametre))
          case _ => false

      case LambdaTerm.App(t1, t2) =>
        // Pour une application, on infère le type de t1 qui doit être une fonction
        infererType(t1, environnement) match
          case Some(LambdaType.FunctionType(typeEntree, typeSortie)) =>
            // On vérifie que t2 a bien le type d'entrée de la fonction
            verifierType(t2, typeEntree, environnement) && typeSortie == typeAttendu
          case _ => false

  // Deuxième fonction: inférer le type d'un terme
  def infererType(terme: LambdaTerm, environnement: Environnement = Map()): Option[LambdaType] =
    terme match
      case LambdaTerm.Var(nomVariable) =>
        // On cherche le type de la variable dans l'environnement
        environnement.get(nomVariable)

      case LambdaTerm.Abs(nomParametre, typeParametre, corps) =>
        // On infère le type du corps avec le paramètre dans l'environnement
        infererType(corps, environnement + (nomParametre -> typeParametre)) match
          case Some(typeCorps) => Some(LambdaType.FunctionType(typeParametre, typeCorps))
          case None => None

      case LambdaTerm.App(t1, t2) =>
        // On infère le type de t1 (qui doit être une fonction) et t2
        infererType(t1, environnement) match
          case Some(LambdaType.FunctionType(typeEntree, typeSortie)) =>
            // On vérifie que t2 a bien le type d'entrée de la fonction
            infererType(t2, environnement) match
              case Some(typeT2) if typeT2 == typeEntree => Some(typeSortie)
              case _ => None
          case _ => None