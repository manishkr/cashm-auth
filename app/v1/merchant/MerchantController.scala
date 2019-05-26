package v1.merchant


import datamodel.security.oauth._
import datamodel.{MerchantAuth, User}
import javax.inject.Inject
import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc._
import scalaoauth2.provider.AuthInfo
import utils.Decoder
import v1.auth.{AuthBaseController, AuthControllerComponents}

import scala.concurrent.{ExecutionContext, Future}

class MerchantController @Inject()(cc: AuthControllerComponents)(dataHandler: OAuthDataHandler)(implicit ec: ExecutionContext)
  extends AuthBaseController(cc) with OAuth2Provider {

  override val tokenEndpoint = new OAuthTokenEndpoint
  private val logger = Logger(getClass)

  def index: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("index: ")
    authorize(dataHandler) { authInfo =>
      Future {
        Ok(Json.obj("userId" -> authInfo.user.userId, "mobile" -> authInfo.user.mobile))
      }
    }
  }

  def authPair: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("index: ")
    authorize(dataHandler) { authInfo =>
      MerchantAuth.get(MerchantAuth(authInfo.user.userId)).map { pair =>
        Ok(Json.obj("status" -> "success", "auth" -> pair.jsonify()))
      }
    }
  }

  def tokenUser: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("tokenUser: ")
    //TODO: This need to verified automatically
    //TODO: Have check of otp
    //TODO: Make it part of regular auth call, such that flow become simple
    val result = for {
      authToken <- request.headers.get("auth_token")
      authSecret <- request.headers.get("auth_secret")
    } yield {
      val oAuthPair = OAuthPair(authToken, authSecret)
      OAuthPair.getUser(oAuthPair).map {
        case Some(user) => Ok(Json.obj("user_id" -> user))
        case _ => Unauthorized
      }
    }

    result match {
      case Some(res) => res
      case _ => Future(Unauthorized)
    }
  }

  def userAuth: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("userAuth: ")
    val params = request.queryString.map { case (k, v) => k -> v.mkString }
    //TODO: This need to verified automatically
    //TODO: Have check of otp
    //TODO: Make it part of regular auth call, such that flow become simple
    println( params.get("mobile"))
    val result = for {
      mobile <- params.get("mobile")
    } yield {
      for {
        authToken <- request.headers.get("auth_token")
        authSecret <- request.headers.get("auth_secret")
      } yield {
        val oAuthPair = OAuthPair(authToken, authSecret)
        for {
          authUser <- OAuthPair.getUser(oAuthPair)
          user <- User.findUserId(Decoder.decodeBase64(mobile))
        } yield {
          authUser match {
            case Some(_) => user match {
              case Some(userId) =>
                dataHandler.createAccessToken( AuthInfo[AccountInfo](AccountInfo(userId, mobile), Some(authToken), None, None), Some(600))
                  .map{token =>
                    Ok(Json.obj("token_type" -> "Bearer", "access_token" -> token.token, "exipres_in" -> token.expiresIn))
                  }
              case _ => Future(BadRequest(Json.obj("status" -> "mobile_not_registered", "message" -> "Mobile number is not registered")))
            }
            case _ => Future(Unauthorized)
          }
        }
      }
    }

    result match {
      case Some(option) => option match {
        case Some(res) => res.flatten
        case _ => Future(Unauthorized)
      }
      case _ => Future(BadRequest(Json.obj("status" -> "mobile_not_found_in_query", "message" -> "Mobile not found in query")))
    }
  }
}
