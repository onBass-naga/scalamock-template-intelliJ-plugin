import org.junit.runner.RunWith
import org.scalatest.WordSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MockTemplateActionSpec extends WordSpec {

  val sut = new MockTemplateAction()

  "MockTemplateAction#generateTemplate" when {
    "curring" should {
      "parse successfully" in {

        val signatureText =
          """
            |def execute[A](
            |  userId: Id[A],
            |  createdAt:     ZonedDateTime
            |)(implicit config: ApplicationConfig): Try[Result]
          """.stripMargin

        val expected =
          """
            |(mock.execute[A](userId: Id[A], createdAt: ZonedDateTime)(implicit config: ApplicationConfig))
            |  .expects(*, *, *)
            |  .once()
            |  .returns(Try[Result])
          """.stripMargin.trim

        val actual = sut.generateTemplate(signatureText)
        assert(actual == expected)
      }
    }

    "signature does not have any arguments" should {
      "parse successfully" in {

        val signatureText =
          """
            |def execute[A]: Try[Result]
          """.stripMargin

        val expected =
          """
            |(mock.execute[A])
            |  .expects()
            |  .once()
            |  .returns(Try[Result])
          """.stripMargin.trim

        val actual = sut.generateTemplate(signatureText)
        assert(actual == expected)
      }
    }

  }
}
