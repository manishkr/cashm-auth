package v1.auth

import datamodel.security.oauth.{OAuth2Provider, OAuthDataHandler, OAuthTokenEndpoint}
import javax.inject.Inject
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class AuthController @Inject()(cc: AuthControllerComponents)(dataHandler: OAuthDataHandler)(implicit ec: ExecutionContext)
    extends AuthBaseController(cc) with OAuth2Provider {

  override val tokenEndpoint = new OAuthTokenEndpoint
  private val logger = Logger(getClass)

  def index: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("index: ")
    authorize(dataHandler) { authInfo =>
      Future{Ok(Json.obj("userId" -> authInfo.user.userId, "mobile" -> authInfo.user.mobile))}
    }
  }
}
