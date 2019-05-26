package datamodel

import scala.concurrent.{ExecutionContext, Future}

case class ChangePassword(oldPassword: String, newPassword: String) {
  def save(userId: String)(implicit ec: ExecutionContext): Future[Boolean] = User.changePassword(userId, oldPassword, newPassword)
}