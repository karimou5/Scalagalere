import org.scalatest.funsuite.AnyFunSuite
import AnalyseurEtendu._
import AnalyseurEtendu.LambdaType._
import AnalyseurEtendu.LambdaTerm._

class AnalyseurEtenduTest extends AnyFunSuite {

  // Test des littéraux et expressions arithmétiques
  test("Typage des littéraux") {
    // Littéraux
    assert(infererType(IntLiteral(42)).isRight)
    assert(infererType(IntLiteral(42)).getOrElse(null) == intType)
    assert(infererType(BoolLiteral(true)).getOrElse(null) == boolType)
    
    // Vérifier directement des littéraux
    assert(verifierType(IntLiteral(42), intType).isRight)
    assert(verifierType(BoolLiteral(true), boolType).isRight)
    
    // Types incompatibles
    assert(verifierType(IntLiteral(42), boolType).isLeft)
    assert(verifierType(BoolLiteral(true), intType).isLeft)
  }
  
  test("Typage des opérations arithmétiques") {
    // Addition
    val addition = Add(IntLiteral(1), IntLiteral(2))
    assert(infererType(addition).getOrElse(null) == intType)
    assert(verifierType(addition, intType).isRight)
    
    // Soustraction
    val soustraction = Sub(IntLiteral(5), IntLiteral(3))
    assert(infererType(soustraction).getOrElse(null) == intType)
    assert(verifierType(soustraction, intType).isRight)
    
    // Multiplication
    val multiplication = Mul(IntLiteral(2), IntLiteral(3))
    assert(infererType(multiplication).getOrElse(null) == intType)
    assert(verifierType(multiplication, intType).isRight)
    
    // Erreurs de typage
    assert(verifierType(Add(IntLiteral(1), BoolLiteral(true)), intType).isLeft)
    assert(verifierType(Sub(BoolLiteral(false), IntLiteral(3)), intType).isLeft)
    
    // Les opérations arithmétiques doivent retourner un entier
    assert(verifierType(Add(IntLiteral(1), IntLiteral(2)), boolType).isLeft)
  }
  
  test("Typage des comparaisons") {
    // Égalité
    val egalite = Eq(IntLiteral(1), IntLiteral(2))
    assert(infererType(egalite).getOrElse(null) == boolType)
    assert(verifierType(egalite, boolType).isRight)
    
    // Inférieur
    val inferieur = Lt(IntLiteral(1), IntLiteral(2))
    assert(infererType(inferieur).getOrElse(null) == boolType)
    assert(verifierType(inferieur, boolType).isRight)
    
    // Les comparaisons peuvent fonctionner sur n'importe quel type comparable avec lui-même
    val boolComp = Eq(BoolLiteral(true), BoolLiteral(false))
    assert(infererType(boolComp).getOrElse(null) == boolType)
    
    // Types incompatibles dans une comparaison
    assert(infererType(Eq(IntLiteral(1), BoolLiteral(true))).isLeft)
    assert(verifierType(Lt(BoolLiteral(false), IntLiteral(3)), boolType).isLeft)
    
    // Les comparaisons doivent retourner un booléen
    assert(verifierType(Eq(IntLiteral(1), IntLiteral(2)), intType).isLeft)
  }
  
  test("Interactions entre lambda calcul et opérations arithmétiques") {
    // Abstraction qui retourne un entier
    val abs = Abs("x", intType, Add(Var("x"), IntLiteral(1)))
    val absType = FunctionType(intType, intType)
    
    assert(infererType(abs).getOrElse(null) == absType)
    assert(verifierType(abs, absType).isRight)
    
    // Application d'une fonction à une expression arithmétique
    val app = App(abs, Mul(IntLiteral(2), IntLiteral(3)))
    assert(infererType(app).getOrElse(null) == intType)
    
    // Fonction qui prend un entier et retourne un booléen (teste si l'entier est positif)
    val isPositive = Abs("x", intType, Lt(IntLiteral(0), Var("x")))
    val isPositiveType = FunctionType(intType, boolType)
    
    assert(infererType(isPositive).getOrElse(null) == isPositiveType)
    
    // Application de isPositive à une expression arithmétique
    val testPositive = App(isPositive, Add(IntLiteral(-5), IntLiteral(10)))
    assert(infererType(testPositive).getOrElse(null) == boolType)
  }
  
  test("Messages d'erreur détaillés") {
    // Test des messages d'erreur avec des informations sur les types
    val erreurAdd = infererType(Add(IntLiteral(1), BoolLiteral(true)))
    assert(erreurAdd.isLeft)
    val msgErreurAdd = erreurAdd.left.getOrElse(null).toString
    assert(msgErreurAdd.contains("incompatible"))
    assert(msgErreurAdd.contains("Int"))
    assert(msgErreurAdd.contains("Bool"))
    
    // Erreur sur un terme plus complexe
    val complexTerm = App(
      Abs("x", intType, Add(Var("x"), IntLiteral(1))),
      BoolLiteral(true)
    )
    val erreurComplex = infererType(complexTerm)
    assert(erreurComplex.isLeft)
    val msgErreurComplex = erreurComplex.left.getOrElse(null).toString
    assert(msgErreurComplex.contains("Type d'argument incompatible"))
  }
  
  test("Propagation d'erreurs") {
    // Vérifier que les erreurs se propagent correctement dans les expressions imbriquées
    val termWithError = Add(
      IntLiteral(1),
      App(
        Abs("x", intType, Var("x")),
        BoolLiteral(true) // Erreur ici: mauvais type d'argument
      )
    )
    
    val result = infererType(termWithError)
    assert(result.isLeft)
    
    // L'erreur devrait mentionner le problème d'application et non l'addition
    val errorMsg = result.left.getOrElse(null).toString
    assert(errorMsg.contains("Type d'argument incompatible"))
  }
}