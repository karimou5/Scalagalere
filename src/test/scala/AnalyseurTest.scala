import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnalyseurTest extends AnyFlatSpec with Matchers {

  import Analyseur.*
  import LambdaType.*
  import LambdaTerm.*

  // Définition de quelques types pour faciliter les tests
  val typeInt = BasicType("Int")
  val typeBool = BasicType("Bool")
  val typeIntToInt = FunctionType(typeInt, typeInt)
  val typeBoolToBool = FunctionType(typeBool, typeBool)

  "La fonction verifierType" should "valider une abstraction simple (identité)" in {
    // λx:Int.x a le type Int -> Int
    val absIdentity = Abs("x", typeInt, Var("x"))
    verifierType(absIdentity, typeIntToInt) shouldBe true
  }

  it should "rejeter une abstraction avec un type incorrect" in {
    // λx:Int.x n'a pas le type Bool -> Bool
    val absIdentity = Abs("x", typeInt, Var("x"))
    verifierType(absIdentity, typeBoolToBool) shouldBe false
  }

  it should "valider une application correcte" in {
    // (λx:Int->Int. x) (λy:Int.y) a le type Int->Int
    val higherOrderId = Abs("x", typeIntToInt, Var("x"))
    val absIntId = Abs("y", typeInt, Var("y"))

    val app = App(higherOrderId, absIntId)
    verifierType(app, typeIntToInt) shouldBe true
  }

  it should "rejeter une application avec des types incompatibles" in {
    // (λx:Bool->Bool. x) (λy:Int.y) est mal typée
    val higherOrderBoolId = Abs("x", typeBoolToBool, Var("x"))
    val absIntId = Abs("y", typeInt, Var("y"))

    val app = App(higherOrderBoolId, absIntId)
    verifierType(app, typeIntToInt) shouldBe false
  }

  "La fonction infererType" should "inférer correctement le type d'une abstraction simple" in {
    // λx:Int.x a le type Int -> Int
    val absIdentity = Abs("x", typeInt, Var("x"))
    infererType(absIdentity) shouldBe Some(typeIntToInt)
  }

  it should "inférer correctement le type d'une application typée" in {
    // (λx:Int->Int. x) (λy:Int.y) a le type Int->Int
    val higherOrderId = Abs("x", typeIntToInt, Var("x"))
    val absIntId = Abs("y", typeInt, Var("y"))

    val app = App(higherOrderId, absIntId)
    infererType(app) shouldBe Some(typeIntToInt)
  }

  it should "retourner None pour un terme mal typé" in {
    // (λx:Bool->Bool. x) (λy:Int.y) est mal typée
    val higherOrderBoolId = Abs("x", typeBoolToBool, Var("x"))
    val absIntId = Abs("y", typeInt, Var("y"))

    val app = App(higherOrderBoolId, absIntId)
    infererType(app) shouldBe None
  }

  it should "inférer correctement le type d'une application composée" in {
    // (λf:Int->Int. λx:Int. f x) a le type (Int->Int) -> Int -> Int
    val appFn = Abs("f", typeIntToInt,
                   Abs("x", typeInt,
                      App(Var("f"), Var("x"))))

    val expectedType = FunctionType(typeIntToInt, FunctionType(typeInt, typeInt))
    infererType(appFn) shouldBe Some(expectedType)
  }

  it should "inférer correctement le type d'une application d'ordre supérieur" in {
    // (λf:Int->Int. λx:Int. f x) (λy:Int.y) a le type Int -> Int
    val appFn = Abs("f", typeIntToInt,
                  Abs("x", typeInt,
                     App(Var("f"), Var("x"))))
    val idFn = Abs("y", typeInt, Var("y"))

    val app = App(appFn, idFn)
    infererType(app) shouldBe Some(FunctionType(typeInt, typeInt))
  }

  it should "gérer correctement les variables libres (non liées)" in {
    // La variable libre z ne devrait pas avoir de type
    val termWithFreeVar = Var("z")
    infererType(termWithFreeVar) shouldBe None
  }

  it should "gérer correctement les abstractions imbriquées" in {
    // λx:Int. λy:Bool. x a le type Int -> Bool -> Int
    val nestedAbs = Abs("x", typeInt,
                      Abs("y", typeBool, Var("x")))

    val expectedType = FunctionType(typeInt, FunctionType(typeBool, typeInt))
    infererType(nestedAbs) shouldBe Some(expectedType)
  }
}