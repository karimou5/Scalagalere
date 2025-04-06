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
  val intToIntType = FunctionType(intType, intType)
  val boolToBoolType = FunctionType(boolType, boolType)

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
    assert(Analyseur.verifierType(identiteInt, intToIntType))

    // λ(x:Int).x ne devrait pas avoir le type Int → Bool
    assert(!Analyseur.verifierType(identiteInt, intToBoolType))

    // λ(x:Int).x ne devrait pas avoir le type Bool → Int
    assert(!Analyseur.verifierType(identiteInt, boolToIntType))

    // λ(x:Int).x ne devrait pas avoir le type de base Int
    assert(!Analyseur.verifierType(identiteInt, intType))
  }

  // Test d'abstraction dans un environnement non-vide
  test("Vérification d'abstraction dans un environnement non-vide") {
    val env = Map("y" -> boolType)

    // λ(x:Int).y où y:Bool dans l'environnement devrait avoir le type Int → Bool
    val absWithEnv = Abs("x", intType, Var("y"))
    assert(Analyseur.verifierType(absWithEnv, intToBoolType, env))

    // Le même terme ne devrait pas avoir le type Int → Int
    assert(!Analyseur.verifierType(absWithEnv, intToIntType, env))
  }

  // Test d'abstraction imbriquée
  test("Vérification d'abstraction imbriquée") {
    // λ(x:Int).λ(y:Bool).x devrait avoir le type Int → (Bool → Int)
    val nestedAbs = Abs("x", intType, Abs("y", boolType, Var("x")))
    val nestedType = FunctionType(intType, FunctionType(boolType, intType))

    assert(Analyseur.verifierType(nestedAbs, nestedType))
    
    // λ(x:Int).λ(y:Bool).y devrait avoir le type Int → (Bool → Bool)
    val nestedAbs2 = Abs("x", intType, Abs("y", boolType, Var("y")))
    val nestedType2 = FunctionType(intType, FunctionType(boolType, boolType))
    
    assert(Analyseur.verifierType(nestedAbs2, nestedType2))
    assert(!Analyseur.verifierType(nestedAbs2, nestedType))
  }

  // Test de vérification d'applications
  test("Vérification de type pour une application") {
    val env = Map("f" -> intToBoolType, "x" -> intType)

    // f x où f:Int→Bool et x:Int devrait avoir le type Bool
    val appFonctionVar = App(Var("f"), Var("x"))
    assert(Analyseur.verifierType(appFonctionVar, boolType, env))

    // f f ne devrait pas être bien typé car f n'est pas de type Int
    val appFonctionFonction = App(Var("f"), Var("f"))
    assert(!Analyseur.verifierType(appFonctionFonction, boolType, env))
  }

  // Test d'application d'abstraction
  test("Vérification d'application d'abstraction") {
    // (λx:Int.x) y où y:Int devrait être de type Int
    val identite = Abs("x", intType, Var("x"))
    val env = Map("y" -> intType)
    val appIdentiteVar = App(identite, Var("y"))
    
    assert(Analyseur.verifierType(appIdentiteVar, intType, env))
    
    // Test avec des abstractions plus complexes
    val constInt = Abs("x", intType, Abs("y", boolType, Var("x")))
    val appConstInt = App(constInt, Var("y"))
    
    // Après application, on obtient λy:Bool.y donc le type est Bool → Int
    assert(Analyseur.verifierType(appConstInt, FunctionType(boolType, intType), env))
  }

  // Tests pour l'inférence de type
  test("Inférence de type pour une variable") {
    val env = Map("x" -> intType, "y" -> boolType)
    
    assert(Analyseur.infererType(Var("x"), env) == Some(intType))
    assert(Analyseur.infererType(Var("y"), env) == Some(boolType))
    assert(Analyseur.infererType(Var("z"), env) == None)
  }

  test("Inférence de type pour une abstraction") {
    // λ(x:Int).x devrait avoir le type Int → Int
    val identiteInt = Abs("x", intType, Var("x"))
    assert(Analyseur.infererType(identiteInt) == Some(intToIntType))
    
    // λ(x:Int).y où y:Bool devrait avoir le type Int → Bool
    val env = Map("y" -> boolType)
    val absWithEnv = Abs("x", intType, Var("y"))
    assert(Analyseur.infererType(absWithEnv, env) == Some(intToBoolType))
    
    // Tests avec noms de variables réutilisés
    val reuseName = Abs("x", intType, Abs("x", boolType, Var("x")))
    assert(Analyseur.infererType(reuseName) == Some(FunctionType(intType, FunctionType(boolType, boolType))))
    
    // La variable x dans le corps fait référence au x le plus proche (ombrage)
    val complexAbs = Abs("x", intType, Abs("y", boolType, App(Var("f"), Var("x"))))
    val envWithF = Map("f" -> intToBoolType)
    assert(Analyseur.infererType(complexAbs, envWithF) == Some(FunctionType(intType, FunctionType(boolType, boolType))))
  }

  test("Inférence de type pour une application") {
    val env = Map("f" -> intToBoolType, "x" -> intType)
    
    // f x où f:Int→Bool et x:Int devrait avoir le type Bool
    val appFonctionVar = App(Var("f"), Var("x"))
    assert(Analyseur.infererType(appFonctionVar, env) == Some(boolType))
    
    // (λx:Int.x) y où y:Int devrait être de type Int
    val identite = Abs("x", intType, Var("x"))
    val appIdentiteVar = App(identite, Var("y"))
    val envWithY = env + ("y" -> intType)
    
    assert(Analyseur.infererType(appIdentiteVar, envWithY) == Some(intType))
    
    // Application mal typée
    val appMalTypee = App(Var("f"), Var("f"))
    assert(Analyseur.infererType(appMalTypee, env) == None)
    
    // Test d'application partielle
    val constFunction = Abs("x", intType, Abs("y", boolType, Var("x")))
    val partialApp = App(constFunction, Var("x"))
    
    assert(Analyseur.infererType(partialApp, env) == Some(FunctionType(boolType, intType)))
  }

  test("Inférence de type pour des termes complexes") {
    // (λf:Int→Bool.λx:Int.f x) (λy:Int.y) z
    // Cette expression ne devrait pas être bien typée car λy:Int.y a le type Int→Int et non Int→Bool
    
    val fonctionHigherOrder = Abs("f", intToBoolType, Abs("x", intType, App(Var("f"), Var("x"))))
    val identiteInt = Abs("y", intType, Var("y"))
    val env = Map("z" -> intType)
    
    val appComplexe = App(App(fonctionHigherOrder, identiteInt), Var("z"))
    assert(Analyseur.infererType(appComplexe, env) == None)
    
    // Mais si on change le type de paramètre de fonctionHigherOrder, ça devrait marcher
    val fonctionHigherOrderCorrigee = Abs("f", intToIntType, Abs("x", intType, App(Var("f"), Var("x"))))
    val appComplexeCorrigee = App(App(fonctionHigherOrderCorrigee, identiteInt), Var("z"))
    assert(Analyseur.infererType(appComplexeCorrigee, env) == Some(intType))
  }
  
  test("Fonctions d'ordre supérieur") {
    // Définir la fonction map: (Int→Bool) → (Int → Bool)
    val mapFunction = Abs("f", intToBoolType, Abs("x", intType, App(Var("f"), Var("x"))))
    val mapFunctionType = FunctionType(intToBoolType, intToBoolType)
    
    assert(Analyseur.infererType(mapFunction) == Some(mapFunctionType))
    
    // Définir une fonction qui prend une fonction et retourne une autre (composition simple)
    val negateFunction = Abs("f", intToBoolType, Abs("x", intType, 
      App(Var("not"), App(Var("f"), Var("x")))
    ))
    val env = Map("not" -> boolToBoolType)
    
    assert(Analyseur.infererType(negateFunction, env) == Some(FunctionType(intToBoolType, intToBoolType)))
  }
  
  test("Fonctions curryfiées") {
    // Fonction add: Int → Int → Int (currifiée)
    val curriedAddType = FunctionType(intType, FunctionType(intType, intType))
    val env = Map("add" -> curriedAddType, "x" -> intType, "y" -> intType)
    
    // add x y
    val addXY = App(App(Var("add"), Var("x")), Var("y"))
    assert(Analyseur.infererType(addXY, env) == Some(intType))
    
    // add x (application partielle)
    val addX = App(Var("add"), Var("x"))
    assert(Analyseur.infererType(addX, env) == Some(FunctionType(intType, intType)))
  }
  
  test("Variables non définies") {
    // Test avec une variable non définie
    val varInconnue = Var("inconnu")
    
    // L'inférence de type devrait échouer pour une variable inconnue
    assert(Analyseur.infererType(varInconnue) == None)
    
    // Même avec des abstractions et applications
    val absAvecVarInconnue = Abs("x", intType, App(Var("f"), Var("x")))
    // f n'est pas défini dans l'environnement
    assert(Analyseur.infererType(absAvecVarInconnue) == None)
  }
}