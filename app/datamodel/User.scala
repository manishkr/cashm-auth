package datamodel

import databases.RedisDBHandler
import org.mindrot.jbcrypt.BCrypt
import play.api.libs.ws.WSClient
import utils.{IdentityUtils, SMSHandler}

import scala.concurrent.{ExecutionContext, Future}

case class User(mobile: String, password: String){
  def save()(implicit ec: ExecutionContext): Future[Boolean] = {
    val userId = IdentityUtils.generateSequentialUniqueId()
    RedisDBHandler.setIfNot(User.collectionName, mobile, userId)
      .map{
        case true => User.setPassword(userId, password)
        case false => Future{false}
      }.flatten
  }
}

object User {
  private val collectionName = "UserMobile"
  private val secretCollectionName = "UserSecret"
  private val forgotPasswordOTPCollectionName = "UserForgotPasswordOTP"
  private val otpExpireTime = 15 * 60
  def findUserId(mobile: String): Future[Option[String]] = {
    RedisDBHandler.get(collectionName, mobile)
  }

  def check(userId : String, password: String)(implicit ec: ExecutionContext): Future[Boolean] = {
    RedisDBHandler.get(User.secretCollectionName, userId)
      .map {
      case Some(storedPassword) => BCrypt.checkpw(password, storedPassword)
      case None => false
    }
  }

  def changePassword(userId: String, oldPassword: String, newPassword: String)(implicit ec: ExecutionContext): Future[Boolean] = {
    check(userId, oldPassword).map {
      case true => setPassword(userId, newPassword)
      case false => Future {
        false
      }
    }.flatten
  }

  def resetPassword(mobile: String, otp: String, newPassword: String)(implicit ec: ExecutionContext): Future[Boolean] = {
    findUserId(mobile).map {
      case Some(userId) => RedisDBHandler.get(forgotPasswordOTPCollectionName, userId).map {
        case Some(storedOTP) => if (storedOTP == otp) setPassword(userId, newPassword) else Future {
          false
        }
        case _ => Future {
          false
        }
      }.flatten
      case _ => Future {
        false
      }
    }.flatten
  }

  def forgotPassword(mobile: String, ws : WSClient)(implicit ec: ExecutionContext): Future[Boolean] = {
    findUserId(mobile).map {
      case Some(userId) => generateAndSendOTP(userId, mobile, ws)
      case _ => Future {
        false
      }
    }.flatten
  }

  def resendOTP(mobile: String, ws : WSClient)(implicit ec: ExecutionContext): Future[Boolean] = {
    findUserId(mobile).map {
      case Some(userId) => {
        RedisDBHandler.get(forgotPasswordOTPCollectionName, userId)
          .map {
            case Some(otp) => SMSHandler.sendOTP(otp, mobile, ws); Future {
              true
            } //send it
            case None => generateAndSendOTP(userId, mobile, ws)
          }.flatten
      }
      case _ => Future {
        false
      }
    }.flatten
  }

  def totalCount()(implicit ec: ExecutionContext) = {
    RedisDBHandler.getAllKeysIn(collectionName)
      .map(_.length)
  }

  private def generateAndSendOTP(userId: String, mobile: String,  ws : WSClient)(implicit ec: ExecutionContext) = {
    val otp = IdentityUtils.generateOTP();
    SMSHandler.sendOTP(otp, mobile, ws)
    RedisDBHandler.setex(forgotPasswordOTPCollectionName, userId, otp, otpExpireTime)
  }

  private def setPassword(userId: String, password: String) = {
    RedisDBHandler.set(User.secretCollectionName, userId, BCrypt.hashpw(password, BCrypt.gensalt()))
  }
}