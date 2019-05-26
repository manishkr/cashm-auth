package datamodel.security.oauth

import play.api.mvc.{ActionBuilder, AnyContent, BaseController}
import scalaoauth2.provider.ProtectedResourceHandler

trait OAuth2ProviderActionBuilders {
  self: BaseController =>

  def AuthorizedAction[U](handler: ProtectedResourceHandler[U]): ActionBuilder[({ type L[A] = AuthInfoRequest[A, U] })#L, AnyContent] = {
    AuthorizedActionFunction(handler)(self.defaultExecutionContext) compose Action
  }

}
