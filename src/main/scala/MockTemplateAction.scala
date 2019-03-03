import scala.util.{Failure, Success}

class MockTemplateAction extends TemplateGenerator {

  def generateTemplate(signatureText: String): String = {
    MethodSignature(signatureText) match {
      case Failure(_) => "[error] failed to generate"
      case Success(s) =>
        s"""
          |(mock.${s.nameAndTypeParameters}${s.arguments.getOrElse("")})
          |  .expects(${s.argumentsMasked})
          |  .once()
          |  .returns(${s.returnType.getOrElse("")})
        """.stripMargin.trim
    }
  }
}
