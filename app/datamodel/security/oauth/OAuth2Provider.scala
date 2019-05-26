package datamodel.security.oauth

import play.api.libs.json._
import play.api.mvc._
import scalaoauth2.provider._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait OAuth2BaseProvider extends Results {

  private[oauth] def getParam[A](request: Request[A]): Map[String, Seq[String]] = {
    (request.body match {
      case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      case body: play.api.mvc.AnyContent if body.asJson.isDefined => FormUtils.fromJson(js = body.asJson.get).mapValues(Seq(_))
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      case body: play.api.libs.json.JsValue => FormUtils.fromJson(js = body).mapValues(Seq(_))
      case _ => Map.empty[String, Seq[String]]
    }) ++ request.queryString
  }

  private[this] object FormUtils {

    import play.api.libs.json._

    def fromJson(prefix: String = "", js: JsValue): Map[String, String] = js match {
      case JsObject(fields) =>
        fields.map { case (key, value) => fromJson(Option(prefix).filterNot(_.isEmpty).map(_ + ".").getOrElse("") + key, value) }.foldLeft(Map.empty[String, String])(_ ++ _)
      case JsArray(values) =>
        values.zipWithIndex.map { case (value, i) => fromJson(prefix + "[" + i + "]", value) }.foldLeft(Map.empty[String, String])(_ ++ _)
      case JsNull => Map.empty
      case JsUndefined() => Map.empty
      case JsBoolean(value) => Map(prefix -> value.toString)
      case JsNumber(value) => Map(prefix -> value.toString)
      case JsString(value) => Map(prefix -> value.toString)
    }

  }

  protected[this] def responseOAuthErrorHeader(e: OAuthError): (String, String) = "WWW-Authenticate" -> ("Bearer " + toOAuthErrorString(e))

  protected def toOAuthErrorString(e: OAuthError): String = {
    val params = Seq("error=\"" + e.errorType + "\"") ++
      (if (!e.description.isEmpty) { Seq("error_description=\"" + e.description + "\"") } else { Nil })
    params.mkString(", ")
  }

}

trait OAuth2ProtectedResourceProvider extends OAuth2BaseProvider {

  val protectedResource: ProtectedResource = ProtectedResource

  implicit def play2protectedResourceRequest(request: RequestHeader): ProtectedResourceRequest = {
    new ProtectedResourceRequest(request.headers.toMap, request.queryString)
  }

  implicit def play2protectedResourceRequest[A](request: Request[A]): ProtectedResourceRequest = {
    val param: Map[String, Seq[String]] = getParam(request)
    new ProtectedResourceRequest(request.headers.toMap, param)
  }

  def authorize[A, U](handler: ProtectedResourceHandler[U])(callback: AuthInfo[U] => Future[Result])(implicit request: Request[A], ctx: ExecutionContext): Future[Result] = {
    protectedResource.handleRequest(request, handler).flatMap {
      case Left(e) => Future.successful(new Status(e.statusCode).withHeaders(responseOAuthErrorHeader(e)))
      case Right(authInfo) => callback(authInfo)
    }
  }
}

trait OAuth2TokenEndpointProvider extends OAuth2BaseProvider {

  val tokenEndpoint: TokenEndpoint = TokenEndpoint

  implicit def play2oauthRequest(request: RequestHeader): AuthorizationRequest = {
    new AuthorizationRequest(request.headers.toMap, request.queryString)
  }

  implicit def play2oauthRequest[A](request: Request[A]): AuthorizationRequest = {
    val param: Map[String, Seq[String]] = getParam(request)
    new AuthorizationRequest(request.headers.toMap, param)
  }

  def issueAccessToken[A, U](handler: AuthorizationHandler[U])(implicit request: Request[A], ctx: ExecutionContext): Future[Result] = {
    tokenEndpoint.handleRequest(request, handler).map {
      case Left(e) => new Status(e.statusCode)(responseOAuthErrorJson(e)).withHeaders(responseOAuthErrorHeader(e))
      case Right(r) => Ok(Json.toJson(responseAccessToken(r))).withHeaders("Cache-Control" -> "no-store", "Pragma" -> "no-cache")
    }
  }

  protected def responseOAuthErrorJson(e: OAuthError): JsValue = Json.obj(
    "error" -> e.errorType,
    "error_description" -> e.description)

  protected def responseAccessToken[U](r: GrantHandlerResult[U]) = {
    Map[String, JsValue](
      "token_type" -> JsString(r.tokenType),
      "access_token" -> JsString(r.accessToken)) ++ r.expiresIn.map {
        "expires_in" -> JsNumber(_)
      } ++ r.refreshToken.map {
        "refresh_token" -> JsString(_)
      } ++ r.scope.map {
        "scope" -> JsString(_)
      } ++ r.params.map(e => (e._1, JsString(e._2)))
  }

}

trait OAuth2Provider extends OAuth2ProtectedResourceProvider with OAuth2TokenEndpointProvider
