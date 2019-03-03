import scala.util.{Failure, Success}

class StubTemplateAction extends TemplateGenerator {

  def generateTemplate(signatureText: String): String = {
    MethodSignature(signatureText) match {
      case Failure(_) => "[error] failed to generate"
      case Success(s) =>
        s"""
          |(stub.${s.nameAndTypeParameters}${s.arguments.getOrElse("")})
          |  .when(${s.argumentsMasked})
          |  .returns(${s.returnType.getOrElse("")})
        """.stripMargin.trim
    }
  }
}
