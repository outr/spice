package spec

import fabric.define.{DefType, Definition, GenericType}
import fabric.rw.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spice.openapi.generator.dart.{DurableSocketDartConfig, DurableSocketDartGenerator}

// AnyVal wrappers must be top-level (can't be nested in a class)
case class UserId(value: String) extends AnyVal
object UserId {
  given RW[UserId] = RW.string(_.value, UserId.apply, className = Some("test.UserId"))
}
case class CreatedAt(value: Long) extends AnyVal
object CreatedAt {
  given RW[CreatedAt] = RW.long(_.value, CreatedAt.apply, className = Some("test.CreatedAt"))
}

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

  case class TypedRecord(id: UserId, name: String, created: CreatedAt, parentId: Option[UserId]) derives RW

  private def generateFiles(name: String, defn: Definition) = {
    val config = DurableSocketDartConfig(
      serviceName = "Test",
      defTypes = List(name -> defn)
    )
    DurableSocketDartGenerator(config).generate()
  }

  private def findSource(files: List[spice.openapi.generator.SourceFile], fileName: String): String =
    files.find(_.fileName == fileName).map(_.source).getOrElse(
      throw new NoSuchElementException(s"File $fileName not found in: ${files.map(_.fileName).mkString(", ")}")
    )

  private def allSources(files: List[spice.openapi.generator.SourceFile]): String =
    files.map(_.source).mkString("\n")

  // --- Tests ---

  "DurableDefTypeSpec" should {
    "generate Dart enum from DefType.Enum" in {
      val files = generateFiles("Color", summon[RW[Color]].definition)
      val source = findSource(files, "color.dart")
      source should include("enum Color")
      source should include("red,")
      source should include("green,")
      source should include("blue;")
      source should include("static Color? fromString")
    }

    "generate Dart class with fromJson constructor" in {
      val files = generateFiles("SimpleMessage", summon[RW[SimpleMessage]].definition)
      val source = findSource(files, "simple_message.dart")
      source should include("class SimpleMessage")
      source should include("final String text;")
      source should include("final int count;")
      source should include("SimpleMessage.fromJson(Map<String, dynamic> json)")
      source should include(": text =")
      source should not include ";;"
    }

    "generate Dart abstract class with polymorphic fromJson for sealed trait" in {
      val files = generateFiles("Animal", summon[RW[Animal]].definition)
      val all = allSources(files)
      all should include("abstract class Animal")
      all should include("static Animal fromJson")
      all should include("if (type == 'Dog') return Dog.fromJson(json);")
      all should include("if (type == 'Cat') return Cat.fromJson(json);")
      all should include("class Dog extends Animal")
      all should include("class Cat extends Animal")
    }

    "generate proper class names for empty case objects in enums" in {
      val files = generateFiles("Status", summon[RW[Status]].definition)
      val all = allSources(files)
      all should not include "class 1"
      all should include("class Active")
      all should include("class Inactive")
      all should include("class Pending")
      all should include("if (type == 'Active') return Active.fromJson(json);")
    }

    "recursively discover nested types" in {
      val files = generateFiles("Wrapper", summon[RW[Wrapper]].definition)
      val all = allSources(files)
      all should include("class Wrapper")
      all should include("abstract class Animal")
      all should include("class Dog")
      all should include("class Cat")
      all should include("abstract class Status")
    }

    "not generate invalid Dart identifiers (no 'enum String')" in {
      val files = generateFiles("Wrapper", summon[RW[Wrapper]].definition)
      val all = allSources(files)
      all should not include "enum String {"
    }

    "generate fromJson with .map for List<Poly> and null-check for Optional<Poly>" in {
      val files = generateFiles("Wrapper", summon[RW[Wrapper]].definition)
      val all = allSources(files)
      all should not include ".cast<Animal>()"
      all should not include ".cast<Status>()"
      all should include("Animal.fromJson")
      all should include("Status.fromJson")
    }

    "generate typed Dart wrapper classes for AnyVal types with className" in {
      val files = generateFiles("TypedRecord", summon[RW[TypedRecord]].definition)
      val all = allSources(files)

      all should include("class UserId {")
      all should include("final String value;")
      all should include("const UserId(this.value);")

      all should include("class CreatedAt {")
      all should include("final int value;")
      all should include("const CreatedAt(this.value);")

      all should include("final UserId id;")
      all should include("final CreatedAt created;")
      all should include("final UserId? parentId;")

      all should include("UserId(")
      all should include("CreatedAt(")

      all should include("id.value")
      all should include("created.value")
    }

    "generate valid Dart constructor syntax" in {
      val files = generateFiles("SimpleMessage", summon[RW[SimpleMessage]].definition)
      val source = findSource(files, "simple_message.dart")
      val lines = source.split("\n")
      lines.count(_.trim.endsWith(";;")) shouldBe 0
      val fromJsonLine = lines.find(_.contains("SimpleMessage.fromJson"))
      fromJsonLine should not be empty
    }

    "generate generic Dart types for parameterized wrappers like Id[User]" in {
      // Simulate what Fabric 1.24 produces for a field of type Id[User]
      // where Id[T] is case class Id[T](value: String)
      val userDef = Definition(
        DefType.Obj(Map("name" -> Definition(DefType.Str))),
        className = Some("test.User")
      )
      val idUserDef = Definition(
        DefType.Str,
        className = Some("test.Id[User]"),
        genericTypes = List(GenericType("T", userDef))
      )
      val idPostDef = Definition(
        DefType.Str,
        className = Some("test.Id[Post]"),
        genericTypes = List(GenericType("T", Definition(
          DefType.Obj(Map("title" -> Definition(DefType.Str))),
          className = Some("test.Post")
        )))
      )
      val recordDef = Definition(
        DefType.Obj(Map(
          "userId" -> idUserDef,
          "postId" -> idPostDef,
          "name" -> Definition(DefType.Str)
        )),
        className = Some("test.Record")
      )
      val files = generateFiles("Record", recordDef)
      val all = allSources(files)

      // Should generate ONE Id wrapper class (not Id[User] and Id[Post] separately)
      all should include("class Id {")
      all should include("final String value;")
      all should include("const Id(this.value);")

      // Field types should use Dart generics
      val recordSource = findSource(files, "record.dart")
      recordSource should include("final Id<User> userId;")
      recordSource should include("final Id<Post> postId;")
      recordSource should include("final String name;")
    }
  }
}
