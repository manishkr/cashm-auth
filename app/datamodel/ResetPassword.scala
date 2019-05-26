package datamodel

import scala.concurrent.{ExecutionContext, Future}

case class ResetPassword(mobile: String, otp: String, newPassword: String) {
  def save()(implicit ec: ExecutionContext): Future[Boolean] = User.resetPassword(mobile, otp, newPassword)
}