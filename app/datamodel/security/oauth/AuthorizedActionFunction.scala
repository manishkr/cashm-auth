package datamodel.security.oauth

import play.api.mvc._
import scalaoauth2.provider.ProtectedResourceHandler

import scala.concurrent.{ExecutionContext, Future}

case class AuthorizedActionFunction[U](handler: ProtectedResourceHandler[U])(implicit ctx: ExecutionContext)
  extends ActionFunction[Request, ({ type L[A] = AuthInfoRequest[A, U] })#L] with OAuth2Provider {

  override protected def executionContext: ExecutionContext = ctx

  override def invokeBlock[A](request: Request[A], block: AuthInfoRequest[A, U] => Future[Result]): Future[Result] = {
    authorize(handler) { authInfo =>
      block(AuthInfoRequest(authInfo, request))
    }(request, ctx)
  }

}
