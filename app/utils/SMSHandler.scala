package utils

import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import scala.concurrent.{ExecutionContext, Future}

object SMSHandler {
  val smsGatwayURL = "http://api.msg91.com/api/v2/sendsms"
  val authKey = "222155A8r9wq5tx5b2e9929"
  def sendOTP(otp : String, mobile : String, ws : WSClient)(implicit ec: ExecutionContext): Future[Unit] = {
    val message = s"Your otp for reset+password is $otp"
    sendOTP(otp, mobile, ws, message)
  }

  def sendVerificationOTP(otp : String, mobile : String, ws : WSClient)(implicit ec: ExecutionContext): Future[Unit] = {
    val message = s"Your otp to verify mobile is $otp"
    sendOTP(otp, mobile, ws, message)
  }

  private def sendOTP(otp: String, mobile: String, ws: WSClient, message: String)(implicit ec: ExecutionContext) = {
    val mobileNumber = mobile.takeRight(10)
    val messageJson = Json.obj("message" -> message, "to" -> List(mobileNumber))
    val json = Json.obj("sender" -> "MKUBER", "route" -> "4", "country" -> "91", "sms" -> List(messageJson))
    ws.url(smsGatwayURL)
      .withHttpHeaders("content-type" -> "application/json", "authkey" -> authKey)
      .post(json).map { response =>
      println(response.statusText)
      println(response.json)
    }
  }
}
