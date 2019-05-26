package datamodel.security.oauth

import play.api.mvc.{Request, WrappedRequest}
import scalaoauth2.provider.AuthInfo

case class AuthInfoRequest[A, U](authInfo: AuthInfo[U], private val request: Request[A]) extends WrappedRequest[A](request)
