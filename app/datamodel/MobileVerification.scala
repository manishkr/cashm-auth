package datamodel

import databases.RedisDBHandler
import play.api.libs.ws.WSClient
import utils.{IdentityUtils, SMSHandler}

import scala.concurrent.{ExecutionContext, Future}

case class MobileVerification(mobile: String, otp : String) {
  def verify()(implicit ec: ExecutionContext): Future[Boolean] = {
    RedisDBHandler.get(MobileVerification.otpCollectionName, mobile)
      .map {
        case Some(dbOTP) => RedisDBHandler.set(MobileVerification.collectionName, mobile, (dbOTP == otp).toString); dbOTP == otp
        case _ => false
      }
  }
}

object MobileVerification{
  val collectionName = "MobileVerification"
  val otpCollectionName = "MobileVerificationOTP"
  private val otpExpireTime = 15 * 60
  def isVerified(mobile : String)(implicit ec: ExecutionContext): Future[Boolean] = {
    RedisDBHandler.get(MobileVerification.collectionName, mobile)
      .map {
        case Some(status) => status == "true"
        case None => false
      }
  }

  def generateAndSendOTP(mobile : String, ws : WSClient)(implicit ec: ExecutionContext): Future[Boolean] = {
    isVerified(mobile)
      .map {
        case true => false
        case false => {
          doGenerateAndSendOTP(mobile, ws)
          true
        }
      }
  }

  private def doGenerateAndSendOTP(mobile: String, ws: WSClient)(implicit ec: ExecutionContext) = {
    val otp = IdentityUtils.generateOTP()
    RedisDBHandler.setex(otpCollectionName, mobile, otp, otpExpireTime)
    SMSHandler.sendVerificationOTP(otp, mobile, ws)
  }

  def resendOTP(mobile : String, ws : WSClient)(implicit ec: ExecutionContext): Future[Boolean] = {
    isVerified(mobile)
      .map {
        case true => false
        case false => {
          RedisDBHandler.get(otpCollectionName, mobile)
            .map {
              case Some(otp) => SMSHandler.sendVerificationOTP(otp, mobile, ws)
              case _ => doGenerateAndSendOTP(mobile, ws)
            }
          true
        }
      }
  }
}