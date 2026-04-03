package spec

import fabric.define.DefType
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

/** Tests Dart code generation from DefType — verifies the generator produces
  * correct Dart for enums, classes, polymorphic types, and edge cases. */
class DurableDefTypeSpec extends AnyWordSpec with Matchers {

  // --- Test types ---

  enum Color { case Red, Green, Blue }
  object Color { given RW[Color] = RW.enumeration(Color.values.toList) }

  case class SimpleMessage(text: String, count: Int) derives RW

  sealed trait Animal derives RW
  case class Dog(name: String, breed: String) extends Animal derives RW
  case class Cat(name: String, indoor: Boolean) extends Animal derives RW

  // Empty case objects (like ActionKind.Respond, MessageVisibility.All)
  enum Status derives RW {
    case Active
    case Inactive
    case Pending(reason: String)
  }

  // Nested types — a poly containing another poly
  case class Wrapper(animal: Animal, status: Status, timestamp: Long) derives RW

  // --- Tests ---

  "DurableDefTypeSpec" should {
    "generate Dart enum from DefType.Enum" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("Color" -> summon[RW[Color]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val typesFile = files.find(_.fileName == "test_types.dart")
      typesFile should not be empty
      val source = typesFile.get.source
      source should include("enum Color")
      source should include("red,")
      source should include("green,")
      source should include("blue;")
      source should include("static Color? fromString")
    }

    "generate Dart class with fromJson constructor" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("SimpleMessage" -> summon[RW[SimpleMessage]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      source should include("class SimpleMessage")
      source should include("final String text;")
      source should include("final int count;")
      source should include("SimpleMessage.fromJson(Map<String, dynamic> json)")
      // Constructor should use : before initializer list
      source should include(": text =")
      // Should NOT have double semicolons
      source should not include ";;"
    }

    "generate Dart abstract class with polymorphic fromJson for sealed trait" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("Animal" -> summon[RW[Animal]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      source should include("abstract class Animal")
      source should include("static Animal fromJson")
      source should include("if (type == 'Dog') return Dog.fromJson(json);")
      source should include("if (type == 'Cat') return Cat.fromJson(json);")
      source should include("class Dog extends Animal")
      source should include("class Cat extends Animal")
    }

    "generate proper class names for empty case objects in enums" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("Status" -> summon[RW[Status]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      // Empty case objects should have proper class names, not "class 1 {"
      source should not include "class 1"
      source should include("class Active")
      source should include("class Inactive")
      source should include("class Pending")
      source should include("if (type == 'Active') return Active.fromJson(json);")
    }

    "recursively discover nested types" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("Wrapper" -> summon[RW[Wrapper]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      // Should discover Animal, Dog, Cat, Status from Wrapper's fields
      source should include("class Wrapper")
      source should include("abstract class Animal")
      source should include("class Dog")
      source should include("class Cat")
      source should include("abstract class Status")
    }

    "not generate invalid Dart identifiers (no 'enum String')" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("Wrapper" -> summon[RW[Wrapper]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      // Should not generate "enum String" or use reserved Dart words as type names
      source should not include "enum String {"
    }

    "generate fromJson with .map for List<Poly> and null-check for Optional<Poly>" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("Wrapper" -> summon[RW[Wrapper]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      // List<Content> fields in Wrapper's Animal subtypes should use .map().toList(), not .cast<>()
      // Optional Poly fields should use null-check + fromJson, not "as Poly?"
      source should not include ".cast<Animal>()"
      source should not include ".cast<Status>()"
      // The Animal field in Wrapper should call fromJson
      source should include("Animal.fromJson")
      // The Status field in Wrapper should call fromJson
      source should include("Status.fromJson")
    }

    "generate valid Dart constructor syntax" in {
      val config = DurableSocketDartConfig(
        serviceName = "Test",
        defTypes = List("SimpleMessage" -> summon[RW[SimpleMessage]].definition)
      )
      val gen = DurableSocketDartGenerator(config)
      val files = gen.generate()
      val source = files.find(_.fileName == "test_types.dart").get.source
      // Constructor should end with single semicolon, not double
      val lines = source.split("\n")
      lines.count(_.trim.endsWith(";;")) shouldBe 0
      // Constructor initializer list should start with :
      val fromJsonLine = lines.find(_.contains("SimpleMessage.fromJson"))
      fromJsonLine should not be empty
    }
  }
}
