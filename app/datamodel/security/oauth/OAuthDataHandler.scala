package datamodel.security.oauth

import java.util.{Date, UUID}

import databases.RedisDBHandler
import datamodel.User
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, Json, _}
import scalaoauth2.provider._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class OAuthDataHandler extends DataHandler[AccountInfo] {
  val accessTokenCollection = "OAuth:access_token"
  val refreshTokenCollection = "OAuth:refresh_token"
  val accessTokenExpire = Some(24* 60 * 60L)

  def validateClient(maybeClientCredential: Option[ClientCredential], request: AuthorizationRequest): Future[Boolean] = {
    // TODO validate client
    Future.successful(true)
  }

  def findUser(maybeClientCredential: Option[ClientCredential], request: AuthorizationRequest): Future[Option[AccountInfo]] = {
    {
      {
        for {
          mobile <- request.param("mobile")
          password <- request.param("password")
        } yield {
          User.findUserId(mobile)
            .map {
              case Some(userId) => User.check(userId, password)
                .map {
                  case true => Some(AccountInfo(userId, mobile))
                  case false => None
                }
              case None => Future.successful(None)
            }.flatten
        }
      } match {
        case Some(future) => future.map(Some(_))
        case None => Future(None)
      }
    }.map(_.flatten)
  }

  def createAccessToken(authInfo: AuthInfo[AccountInfo]): Future[AccessToken] = {
    createAccessToken(authInfo, accessTokenExpire)
  }

  def createAccessToken(authInfo: AuthInfo[AccountInfo], tokenExpireTime : Option[Long]): Future[AccessToken] = {
    val refreshToken = Some(UUID.randomUUID().toString)
    val accessToken = UUID.randomUUID().toString
    val now = DateTime.now().toDate

    val tokenObject = AccessToken(accessToken, refreshToken, authInfo.scope, tokenExpireTime, now)
    saveToken(authInfo, tokenObject)

    Future.successful(tokenObject)
  }

  def getStoredAccessToken(authInfo: AuthInfo[AccountInfo]): Future[Option[AccessToken]] = {
    Future.successful(getAccessToken(authInfo.user.userId, authInfo.clientId.get) match {
      case Some(token) if token.scope.equals(authInfo.scope) => Some(token)
      case _ => None // no previous token or scope changed
    })
  }

  private def getAccessToken(username: String, clientId: String) = {
    val future = RedisDBHandler.get(accessTokenCollection, key(username, clientId)).map {
      case Some(data) => Json.parse(data).validate[AccessToken].asOpt
      case _ => None
    }

    Await.result(future, 1 seconds)
  }

  private def saveToken(authInfo: AuthInfo[AccountInfo], tokenObject: AccessToken) = {
    val username = authInfo.user.userId
    val clientId = authInfo.clientId.get

    for (existing <- getAccessToken(username, clientId)) {
      RedisDBHandler.delete(accessTokenCollection, existing.token)
      RedisDBHandler.delete(refreshTokenCollection, existing.refreshToken.get)
    }

    RedisDBHandler.setex(accessTokenCollection, key(username, clientId), Json.stringify(Json.toJson(tokenObject)), tokenObject.expiresIn.get.toInt)

    RedisDBHandler.setex(accessTokenCollection, tokenObject.token, Json.stringify(Json.toJson(authInfo)), tokenObject.expiresIn.get.toInt)
    RedisDBHandler.setex(refreshTokenCollection, tokenObject.refreshToken.get, Json.stringify(Json.toJson(authInfo)), tokenObject.expiresIn.get.toInt)
  }

  def refreshAccessToken(authInfo: AuthInfo[AccountInfo], refreshToken: String): Future[AccessToken] = {
    val accessToken = UUID.randomUUID().toString
    val now = DateTime.now().toDate
    val tokenObject = AccessToken(accessToken, Some(refreshToken), authInfo.scope, accessTokenExpire, now)

    saveToken(authInfo, tokenObject)

    Future.successful(tokenObject)
  }

  def findAccessToken(token: String): Future[Option[AccessToken]] = {
    val future = RedisDBHandler.get(accessTokenCollection, token).map {
      case Some(data) => Json.parse(data).validate[AuthInfo[AccountInfo]].asOpt
      case _ => None
    }

    val futureInfo = future.map {
      case Some(authInfo) => {
        val fut = RedisDBHandler.get(accessTokenCollection, key(authInfo.user.userId, authInfo.clientId.get))
        Await.result(fut, 1 seconds)
      }
      case _ => None
    }

    futureInfo.map {
      case Some(data) => Json.parse(data).validate[AccessToken].asOpt
      case _ => None
    }
  }

  def findAuthInfoByAccessToken(accessToken: AccessToken): Future[Option[AuthInfo[AccountInfo]]] = {
    RedisDBHandler.get(accessTokenCollection, accessToken.token).map {
      case Some(data) => Json.parse(data).validate[AuthInfo[AccountInfo]].asOpt
      case _ => None
    }
  }

  def findAuthInfoByRefreshToken(refreshToken: String): Future[Option[AuthInfo[AccountInfo]]] = {
    RedisDBHandler.get(refreshTokenCollection, refreshToken).map {
      case Some(data) => Json.parse(data).validate[AuthInfo[AccountInfo]].asOpt
      case _ => None
    }
  }

  def findAuthInfoByCode(code: String): Future[Option[AuthInfo[AccountInfo]]] = {
    Future.failed(new NotImplementedError)
  }

  implicit val tokenFormat: OFormat[AccessToken] = Json.format[AccessToken]
  implicit val accountInfoFormat: OFormat[AccountInfo] = Json.format[AccountInfo]

  implicit val authInfoFormat: Format[AuthInfo[AccountInfo]] =
    ((__ \ "user").format[AccountInfo] ~
      (__ \ "clientId").formatNullable[String] ~
      (__ \ "scope").formatNullable[String] ~
      (__ \ "redirectUri").formatNullable[String]) (AuthInfo.apply, unlift(AuthInfo.unapply))
  implicit val accessTokenFormat: Format[AccessToken] =
    ((__ \ "token").format[String] ~
      (__ \ "refreshToken").formatNullable[String] ~
      (__ \ "scope").formatNullable[String] ~
      (__ \ "lifeSeconds").formatNullable[Long] ~
      (__ \ "createdAt").format[Date] ~
      (__ \ "params").format[Map[String, String]]) (AccessToken.apply, unlift(AccessToken.unapply))
  
  def deleteAuthCode(code: String): Future[Unit] = {
    Future.failed(new NotImplementedError)
  }

  def deleteAuthKey(authInfo : AuthInfo[AccountInfo]) = {
    val username = authInfo.user.userId
    val clientId = authInfo.clientId.get

    for (existing <- getAccessToken(username, clientId)) {
      RedisDBHandler.delete(accessTokenCollection, key(username, clientId))
      RedisDBHandler.delete(accessTokenCollection, existing.token)
      RedisDBHandler.delete(refreshTokenCollection, existing.refreshToken.get)
    }
  }

  def key(username: String, clientId: String) = s"$username:$clientId"
}

