import org.scalatest.flatspec.AnyFlatSpec

class HelloWorldTest extends AnyFlatSpec {

  "La fonction helloWorld" should "renvoyer 'Hello World'" in {
    assert(HelloWorld.helloWorld() == "Hello World")
  }
}
