import org.scalatest.funsuite.AnyFunSuite
import Analyseur._
import Analyseur.LambdaType._
import Analyseur.LambdaTerm._

class AnalyseurTest extends AnyFunSuite {

  // Définition de types communs
  val intType = BasicType("Int")
  val boolType = BasicType("Bool")
  val intToBoolType = FunctionType(intType, boolType)
  val boolToIntType = FunctionType(boolType, intType)

  // Test de vérification de variables
  test("Vérification de type pour une variable") {
    val env = Map("x" -> intType, "y" -> boolType)

    // Variable existante avec bon type
    assert(Analyseur.verifierType(Var("x"), intType, env))

    // Variable existante avec mauvais type
    assert(!Analyseur.verifierType(Var("x"), boolType, env))

    // Variable non définie dans l'environnement
    assert(!Analyseur.verifierType(Var("z"), intType, env))
  }

  // Test de vérification d'abstractions
  test("Vérification de type pour une abstraction") {
    // λ(x:Int).x devrait avoir le type Int → Int
    val identiteInt = Abs("x", intType, Var("x"))
    assert(Analyseur.verifierType(identiteInt, FunctionType(intType, intType)))

    // λ(x:Int).x ne devrait pas avoir le type Int → Bool
    assert(!Analyseur.verifierType(identiteInt, FunctionType(intType, boolType)))

    // λ(x:Int).x ne devrait pas avoir le type Bool → Int
    assert(!Analyseur.verifierType(identiteInt, FunctionType(boolType, intType)))

    // λ(x:Int).x ne devrait pas avoir le type de base Int
    assert(!Analyseur.verifierType(identiteInt, intType))
  }

  // Test plus complexe avec un environnement
  test("Vérification d'abstraction dans un environnement non-vide") {
    val env = Map("y" -> boolType)

    // λ(x:Int).y où y:Bool dans l'environnement devrait avoir le type Int → Bool
    val absWithEnv = Abs("x", intType, Var("y"))
    assert(Analyseur.verifierType(absWithEnv, FunctionType(intType, boolType), env))

    // Le même terme ne devrait pas avoir le type Int → Int
    assert(!Analyseur.verifierType(absWithEnv, FunctionType(intType, intType), env))
  }

  // Test pour une abstraction imbriquée
  test("Vérification d'abstraction imbriquée") {
    // λ(x:Int).λ(y:Bool).x devrait avoir le type Int → (Bool → Int)
    val nestedAbs = Abs("x", intType, Abs("y", boolType, Var("x")))
    val nestedType = FunctionType(intType, FunctionType(boolType, intType))

    assert(Analyseur.verifierType(nestedAbs, nestedType))
  }
}