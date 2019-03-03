import org.junit.runner.RunWith
import org.scalatest.{FreeSpec, WordSpec}
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MethodSignatureSpec extends FreeSpec {

  "MethodSignature" - {

    "文字列で渡されるメソッドシグネチャをパースできる" in {
      val signatureText =
        """
            |def execute[A](
            |  userId: Id[A],
            |  createdAt: ZonedDateTime
            |)(implicit config: ApplicationConfig): Try[Result]
          """.stripMargin

      val actual = MethodSignature(signatureText).get
      assert(actual.nameAndTypeParameters == "execute[A]")
      assert(
        actual.arguments.get.toString == "(userId: Id[A], createdAt: ZonedDateTime)(implicit config: ApplicationConfig)"
      )
      assert(actual.returnType.get.value == "Try[Result]")
    }

    "型パラメータにスペースが含まれていてもパースできる" in {

      val signatureText =
        """
            |def execute[A,  M](
            |  userId: Id[A],
            |  orderId: Id[M],
            |  createdAt: ZonedDateTime
            |): Try[Result]
          """.stripMargin

      val actual = MethodSignature(signatureText).get
      assert(actual.nameAndTypeParameters == "execute[A, M]")
      assert(
        actual.arguments.get.toString == "(userId: Id[A], orderId: Id[M], createdAt: ZonedDateTime)"
      )
      assert(actual.returnType.get.value == "Try[Result]")
    }

    "第一引数が空であってもパースできる" in {

      val signatureText =
        """
            |def execute[A]()(implicit config: ApplicationConfig): Try[Result]
          """.stripMargin

      val actual = MethodSignature(signatureText).get
      assert(actual.nameAndTypeParameters == "execute[A]")
      assert(actual.arguments.get.toString == "()(implicit config: ApplicationConfig)")
      assert(actual.returnType.get.value == "Try[Result]")
    }

    "戻り値の型表記がない場合もパースできる" in {

      val signatureText =
        """
            |def execute[A](
            |  userId: Id[A],
            |  createdAt: ZonedDateTime
            |)(implicit config: ApplicationConfig)
          """.stripMargin

      val actual = MethodSignature(signatureText).get
      assert(actual.nameAndTypeParameters == "execute[A]")
      assert(
        actual.arguments.get.toString == "(userId: Id[A], createdAt: ZonedDateTime)(implicit config: ApplicationConfig)"
      )
      assert(actual.returnType.isEmpty)
    }

    "引数がない場合もパースできる" in {

      val signatureText = "def toString: String"
      val actual        = MethodSignature(signatureText).get
      assert(actual.nameAndTypeParameters == "toString")
      assert(actual.returnType.get.value == "String")
    }

    "引数も戻り値表記もない場合もパースできる" in {

      val signatureText = "def toString"
      val actual        = MethodSignature(signatureText).get
      assert(actual.nameAndTypeParameters == "toString")
      assert(actual.returnType.isEmpty)
    }
  }
}
