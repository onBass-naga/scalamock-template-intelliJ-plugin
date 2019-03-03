import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

case class MethodSignature(
    nameAndTypeParameters: String,
    arguments: Option[Arguments],
    returnType: Option[Type]
) {

  def argumentsMasked: String = {
    val length = arguments.map(_.countArguments).orElse(Some(0)).get
    Range(0, length).map(_ => "*").mkString(", ")
  }
}

object MethodSignature {

  val Patterns = List(
    Pattern((text: String) => text.contains("(") && text.contains("):"), new Regex("""([^(]+)(\(.*\))[:]?(.*)""")),
    Pattern((text: String) => text.contains("(") && !text.contains("):"), new Regex("""([^(]+)(\(.*\))()""")),
    Pattern((text: String) => !text.contains("("), new Regex("""([^:]+)():?(.*)""")),
  )

  def apply(signature: String): Try[MethodSignature] = {
    val text    = signature.replaceAll("\n", "")
    val pattern = Patterns.find(_.predicate(text)).get
    pattern.regex.findFirstMatchIn(text).map { result =>
      SignatureText(result.group(1), opt(result.group(2)), opt(result.group(3)))
    } match {
      case Some(parsed) => Success(MethodSignature(parsed.name, parsed.arguments, parsed.returnType))
      case _            => Failure(new IllegalArgumentException("cannot parsed"))
    }
  }

  private def opt(text: String) = if (text.isEmpty) None else Some(text)

  case class Pattern(predicate: String => Boolean, regex: Regex)
}

case class Arguments(values: List[List[Argument]] = Nil) {
  override def toString: String =
    values
      .map(argBlocks => {
        argBlocks.map(arg => arg.value).mkString("(", ", ", ")")
      })
      .mkString("")

  def countArguments: Int = values.flatten.length
}

case class Argument(value: String) extends AnyVal
object Argument {
  def of(text: String): Argument = {
    if (text.contains(":")) {
      val values = text.split(":")
      Argument(s"${values(0).trim}: ${values(1).trim}")
    } else Argument(text.trim)
  }
}

case class Type(value: String) extends AnyVal {
  override def toString: String = value
}

case class SignatureText(
    namePart: String,
    argumentPart: Option[String],
    returnPart: Option[String]
) {

  private val ArgumentPattern = new Regex("""\((.*?)\)""")

  def name: String = namePart.replaceAll("""\s+""", " ").split("""[^,]\s""").last

  def arguments: Option[Arguments] = {
    argumentPart.map { text =>
      val arguments: List[List[Argument]] = ArgumentPattern
        .findAllMatchIn(text)
        .map(_.group(1).split(",").toList.map(src => Argument.of(src)))
        .toList
      Arguments(arguments)
    }
  }

  def returnType: Option[Type] = returnPart.map(text => Type(text.trim))
}
