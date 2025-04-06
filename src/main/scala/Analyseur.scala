import Analyseur.LambdaTerm.{Abs, Var}
import Analyseur.LambdaType.FunctionType

// Analyseur de types pour le lambda calcul simplement typé
object Analyseur:
  // Types du lambda calcul
  // BasicType("Int") représente le type entier
  // BasicType("Bool") représente le type booléen
  // FunctionType(t1, t2) représente le type fonction de t1 à t2
  // FunctionType(BasicType("Int"), BasicType("Bool")) représente le type fonction de Int à Bool (Int → Bool)
  enum LambdaType:
    case BasicType(s: String)
    case FunctionType(t1: LambdaType, t2: LambdaType)

  // Termes du lambda calcul
  // val variable = LambdaTerm.Var("x")
  // val abstraction = LambdaTerm.Abs("x", intType, LambdaTerm.Var("x"))
  // val application = LambdaTerm.App(abstraction, LambdaTerm.Var("y")) du coup c'est égal à λx:Int.x y
  enum LambdaTerm:
    case Var(varName: String)
    case Abs(varName: String, varType: LambdaType, t: LambdaTerm)
    case App(t1: LambdaTerm, t2: LambdaTerm)

  // Environnement qui represent le  tout ce qu'il y a écrit avant le  ⊢
  type Environnement = Map[String, LambdaType]


  // Première fonction doit vérifier le type d'un terme, prend LambdaTerm et LambdaType return true si LambdaTerm a type = LambdaType
  def verifierType(terme: LambdaTerm, typeAttendu: LambdaType, environnement: Environnement = Map()): Boolean = {
    terme match {
      case Var(nomVariable) =>
        environnement.get(nomVariable) match {
          case Some(typeVariable) => typeVariable == typeAttendu
          case None => false
        }

      case Abs(nomParametre, typeParametre, corps) =>
        // Pour vérifier une abstraction lambda λ(x:T).e, le type attendu doit être un type fonction
        typeAttendu match {
          // On vérifie si typeAttendu est bien un type fonction (A → B)
          case FunctionType(typeEntree, typeSortie) if typeEntree == typeParametre =>
            // Si le type d'entrée correspond au type déclaré du paramètre
            // Par exemple, pour λ(x:N).x+3<5 avec typeAttendu=N→B, on vérifie N==N donc avec le N de λ(x:N)

            // On vérifie ensuite si le corps a le type de sortie attendu
            // On étend l'environnement pour que le corps puisse accéder au paramètre
            // Pour notre exemple: verifierType("x+3<5", B, {x:N})
            verifierType(corps, typeSortie, environnement + (nomParametre -> typeParametre))

          // Si typeAttendu n'est pas un type fonction ou si les types d'entrée ne correspondent pas
          // Par exemple, si on attendait B→N ou Int→B avec λ(x:Bool)...
          case _ => false
        }
    }

    def infererType(terme: LambdaTerm, environnement: Environnement): LambdaType = {
      terme match {
        case Var(nomVariable) =>
          // On cherche le type de la variable dans l'environnement
          environnement.get(nomVariable) match {
            case Some(typeVariable) => typeVariable
            case None => throw new Exception(s"Variable $nomVariable non définie")
          }

        case Abs(nomParametre, typeParametre, corps) =>
          // On infère le type du corps en ajoutant le paramètre à l'environnement
          val typeCorps = infererType(corps, environnement + (nomParametre -> typeParametre))
          FunctionType(typeParametre, typeCorps)

        case App(t1, t2) =>
          // On infère les types des deux termes
          val typeT1 = infererType(t1, environnement)
          val typeT2 = infererType(t2, environnement)

          // On vérifie si t1 est une fonction et si t2 a le bon type d'entrée
          typeT1 match {
            case FunctionType(typeEntree, typeSortie) if typeEntree == typeT2 => typeSortie
            case _ => throw new Exception("Application de fonction invalide")
          }
      }
    }
  }


