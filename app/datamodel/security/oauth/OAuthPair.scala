package datamodel.security.oauth

import com.redis.serialization.Format
import databases.RedisDBHandler
import play.api.libs.json.{JsObject, Json}
import utils.IdentityUtils

import scala.concurrent.Future

case class OAuthPair(authToken : String, authSecret: String){
  implicit val customFormat: Format[OAuthPair] = OAuthPair.customFormat

  def save(userId: String): Future[Boolean] = {
    RedisDBHandler.set(OAuthPair.collectionName, userId, this)
    RedisDBHandler.set(OAuthPair.collectionNameUser, customFormat.write(this), userId)
  }

  def jsonify(): JsObject = Json.obj("auth_token" -> authToken, "auth_secret" -> authSecret)
}

object OAuthPair{
  val collectionName = "OAuthPair"
  val collectionNameUser = "OAuthPairUser"
  implicit val customFormat: Format[OAuthPair] =
    new Format[OAuthPair] {
      def read(str: String): OAuthPair = {
        val list = str.split('|').toList
        val token = list.head
        val secret = list.tail.head

        OAuthPair(token, secret)
      }

      def write(authPair: OAuthPair): String = {
        s"${authPair.authToken}|${authPair.authSecret}"
      }
    }

  def generate() = OAuthPair(IdentityUtils.generateRandomUniqueId(), IdentityUtils.generateRandomUniqueId())

  def get(userId: String): Future[Option[OAuthPair]] = RedisDBHandler.get[OAuthPair](OAuthPair.collectionName, userId)

  def getUser(pair : OAuthPair): Future[Option[String]] = RedisDBHandler.get[String](collectionNameUser, customFormat.write(pair))
}