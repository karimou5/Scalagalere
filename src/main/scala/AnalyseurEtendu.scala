// Analyseur de types étendu avec seulement des expressions arithmétiques
object AnalyseurEtendu:
  enum LambdaType:
    case BasicType(s: String)
    case FunctionType(t1: LambdaType, t2: LambdaType)
    
    override def toString(): String = this match
      case BasicType(s) => s
      case FunctionType(t1, t2) => s"(${t1} → ${t2})"

  enum LambdaTerm:
    case Var(varName: String)
    case Abs(varName: String, varType: LambdaType, t: LambdaTerm)
    case App(t1: LambdaTerm, t2: LambdaTerm)
    case IntLiteral(value: Int)
    case BoolLiteral(value: Boolean)
    case Add(t1: LambdaTerm, t2: LambdaTerm)
    case Sub(t1: LambdaTerm, t2: LambdaTerm)
    case Mul(t1: LambdaTerm, t2: LambdaTerm)
    case Eq(t1: LambdaTerm, t2: LambdaTerm)
    case Lt(t1: LambdaTerm, t2: LambdaTerm)
    
    override def toString(): String = this match
      case Var(name) => name
      case Abs(name, tpe, body) => s"λ$name:$tpe.$body"
      case App(t1, t2) => s"($t1 $t2)"
      case IntLiteral(value) => value.toString
      case BoolLiteral(value) => value.toString
      case Add(t1, t2) => s"($t1 + $t2)"
      case Sub(t1, t2) => s"($t1 - $t2)"
      case Mul(t1, t2) => s"($t1 * $t2)"
      case Eq(t1, t2) => s"($t1 == $t2)"
      case Lt(t1, t2) => s"($t1 < $t2)"

  type Environnement = Map[String, LambdaType]
  
  case class TypeError(message: String, terme: LambdaTerm, typeAttendu: Option[LambdaType] = None, typeInfere: Option[LambdaType] = None):
    override def toString(): String =
      val typeInfo = (typeAttendu, typeInfere) match
        case (Some(expected), Some(inferred)) => s" - attendu: $expected, trouvé: $inferred"
        case (Some(expected), None) => s" - attendu: $expected"
        case (None, Some(inferred)) => s" - trouvé: $inferred"
        case _ => ""
      s"Erreur de typage: $message dans le terme '$terme'$typeInfo"

  val intType: LambdaType = LambdaType.BasicType("Int")
  val boolType: LambdaType = LambdaType.BasicType("Bool")

  def verifierType(terme: LambdaTerm, typeAttendu: LambdaType, environnement: Environnement = Map()): Either[TypeError, Boolean] =
    import LambdaTerm._
    import LambdaType._
    
    terme match
      case Var(nomVariable) =>
        environnement.get(nomVariable) match
          case Some(typeVariable) if typeVariable == typeAttendu => Right(true)
          case Some(typeVariable) => Left(TypeError(s"La variable '$nomVariable' a un type incompatible", terme, Some(typeAttendu), Some(typeVariable)))
          case None => Left(TypeError(s"Variable '$nomVariable' non définie dans l'environnement", terme))

      case Abs(nomParametre, typeParametre, corps) =>
        typeAttendu match
          case FunctionType(typeEntree, typeSortie) if typeEntree == typeParametre =>
            verifierType(corps, typeSortie, environnement + (nomParametre -> typeParametre))
          case FunctionType(typeEntree, _) => 
            Left(TypeError(s"Type de paramètre incompatible", terme, Some(typeEntree), Some(typeParametre)))
          case _ => 
            Left(TypeError("Type fonction attendu pour une abstraction", terme, Some(typeAttendu)))

      case App(t1, t2) =>
        infererType(t1, environnement) match
          case Right(FunctionType(typeEntree, typeSortie)) =>
            if typeSortie != typeAttendu then
              Left(TypeError("Type de retour de fonction incompatible", terme, Some(typeAttendu), Some(typeSortie)))
            else
              verifierType(t2, typeEntree, environnement) match
                case Right(_) => Right(true)
                case Left(erreur) => Left(erreur)
          case Right(autreType) => 
            Left(TypeError("Application sur un terme qui n'est pas une fonction", t1, Some(FunctionType(BasicType("?"), BasicType("?"))), Some(autreType)))
          case Left(erreur) => Left(erreur)

      case IntLiteral(_) =>
        if typeAttendu == intType then Right(true)
        else Left(TypeError("Littéral entier avec type incompatible", terme, Some(typeAttendu), Some(intType)))

      case BoolLiteral(_) =>
        if typeAttendu == boolType then Right(true)
        else Left(TypeError("Littéral booléen avec type incompatible", terme, Some(typeAttendu), Some(boolType)))

      case Add(t1, t2) =>
        if typeAttendu != intType then
          Left(TypeError("Les opérations arithmétiques retournent des entiers", terme, Some(typeAttendu), Some(intType)))
        else
          // Inférer le type des sous-expressions avant de les vérifier directement
          infererType(t1, environnement) match
            case Left(erreur) => Left(erreur)
            case Right(_) => 
              infererType(t2, environnement) match
                case Left(erreur) => Left(erreur)
                case Right(_) => 
                  verifierType(t1, intType, environnement) match
                    case Right(_) => 
                      verifierType(t2, intType, environnement)
                    case Left(erreur) => Left(erreur)
      
      case Sub(t1, t2) =>
        if typeAttendu != intType then
          Left(TypeError("Les opérations arithmétiques retournent des entiers", terme, Some(typeAttendu), Some(intType)))
        else
          // Inférer le type des sous-expressions avant de les vérifier directement
          infererType(t1, environnement) match
            case Left(erreur) => Left(erreur)
            case Right(_) => 
              infererType(t2, environnement) match
                case Left(erreur) => Left(erreur)
                case Right(_) => 
                  verifierType(t1, intType, environnement) match
                    case Right(_) => 
                      verifierType(t2, intType, environnement)
                    case Left(erreur) => Left(erreur)
      
      case Mul(t1, t2) =>
        if typeAttendu != intType then
          Left(TypeError("Les opérations arithmétiques retournent des entiers", terme, Some(typeAttendu), Some(intType)))
        else
          // Inférer le type des sous-expressions avant de les vérifier directement
          infererType(t1, environnement) match
            case Left(erreur) => Left(erreur)
            case Right(_) => 
              infererType(t2, environnement) match
                case Left(erreur) => Left(erreur)
                case Right(_) => 
                  verifierType(t1, intType, environnement) match
                    case Right(_) => 
                      verifierType(t2, intType, environnement)
                    case Left(erreur) => Left(erreur)

      case Eq(t1, t2) =>
        if typeAttendu != boolType then
          Left(TypeError("Les comparaisons retournent des booléens", terme, Some(typeAttendu), Some(boolType)))
        else
          infererType(t1, environnement) match
            case Left(erreur) => Left(erreur)
            case Right(type1) =>
              infererType(t2, environnement) match
                case Left(erreur) => Left(erreur)
                case Right(_) =>
                  verifierType(t2, type1, environnement)
      
      case Lt(t1, t2) =>
        if typeAttendu != boolType then
          Left(TypeError("Les comparaisons retournent des booléens", terme, Some(typeAttendu), Some(boolType)))
        else
          infererType(t1, environnement) match
            case Left(erreur) => Left(erreur)
            case Right(type1) =>
              infererType(t2, environnement) match
                case Left(erreur) => Left(erreur)
                case Right(_) =>
                  verifierType(t2, type1, environnement)

  def infererType(terme: LambdaTerm, environnement: Environnement = Map()): Either[TypeError, LambdaType] =
    import LambdaTerm._
    import LambdaType._
    
    terme match
      case Var(nomVariable) =>
        environnement.get(nomVariable) match
          case Some(typeVariable) => Right(typeVariable)
          case None => Left(TypeError(s"Variable '$nomVariable' non définie dans l'environnement", terme))

      case Abs(nomParametre, typeParametre, corps) =>
        infererType(corps, environnement + (nomParametre -> typeParametre)) match
          case Right(typeCorps) => Right(FunctionType(typeParametre, typeCorps))
          case Left(erreur) => Left(erreur)

      case App(t1, t2) =>
        infererType(t1, environnement) match
          case Right(FunctionType(typeEntree, typeSortie)) =>
            infererType(t2, environnement) match
              case Right(typeT2) if typeT2 == typeEntree => Right(typeSortie)
              case Right(typeT2) => Left(TypeError("Type d'argument incompatible", t2, Some(typeEntree), Some(typeT2)))
              case Left(erreur) => Left(erreur)
          case Right(autreType) => Left(TypeError("Application sur un terme qui n'est pas une fonction", t1, None, Some(autreType)))
          case Left(erreur) => Left(erreur)

      case IntLiteral(_) => Right(intType)
      case BoolLiteral(_) => Right(boolType)

      case Add(t1, t2) =>
        // D'abord, vérifier si les sous-expressions ont des erreurs
        infererType(t1, environnement) match
          case Left(erreur) => Left(erreur)
          case Right(_) =>
            infererType(t2, environnement) match
              case Left(erreur) => Left(erreur)
              case Right(_) =>
                // Ensuite, vérifier les types
                verifierType(t1, intType, environnement) match
                  case Right(_) =>
                    verifierType(t2, intType, environnement) match
                      case Right(_) => Right(intType)
                      case Left(erreur) => Left(erreur)
                  case Left(erreur) => Left(erreur)
      
      case Sub(t1, t2) =>
        // D'abord, vérifier si les sous-expressions ont des erreurs
        infererType(t1, environnement) match
          case Left(erreur) => Left(erreur)
          case Right(_) =>
            infererType(t2, environnement) match
              case Left(erreur) => Left(erreur)
              case Right(_) =>
                // Ensuite, vérifier les types
                verifierType(t1, intType, environnement) match
                  case Right(_) =>
                    verifierType(t2, intType, environnement) match
                      case Right(_) => Right(intType)
                      case Left(erreur) => Left(erreur)
                  case Left(erreur) => Left(erreur)
      
      case Mul(t1, t2) =>
        // D'abord, vérifier si les sous-expressions ont des erreurs
        infererType(t1, environnement) match
          case Left(erreur) => Left(erreur)
          case Right(_) =>
            infererType(t2, environnement) match
              case Left(erreur) => Left(erreur)
              case Right(_) =>
                // Ensuite, vérifier les types
                verifierType(t1, intType, environnement) match
                  case Right(_) =>
                    verifierType(t2, intType, environnement) match
                      case Right(_) => Right(intType)
                      case Left(erreur) => Left(erreur)
                  case Left(erreur) => Left(erreur)

      case Eq(t1, t2) =>
        infererType(t1, environnement) match
          case Left(erreur) => Left(erreur)
          case Right(type1) =>
            infererType(t2, environnement) match
              case Left(erreur) => Left(erreur)
              case Right(_) =>
                verifierType(t2, type1, environnement) match
                  case Right(_) => Right(boolType)
                  case Left(erreur) => Left(erreur)
      
      case Lt(t1, t2) =>
        infererType(t1, environnement) match
          case Left(erreur) => Left(erreur)
          case Right(type1) =>
            infererType(t2, environnement) match
              case Left(erreur) => Left(erreur)
              case Right(_) =>
                verifierType(t2, type1, environnement) match
                  case Right(_) => Right(boolType)
                  case Left(erreur) => Left(erreur)