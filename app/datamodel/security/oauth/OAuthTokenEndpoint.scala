package datamodel.security.oauth

import javax.inject.Singleton

import scalaoauth2.provider._

@Singleton
class OAuthTokenEndpoint extends TokenEndpoint {
  override val handlers = Map(
    OAuthGrantType.AUTHORIZATION_CODE -> new AuthorizationCode(),
    OAuthGrantType.REFRESH_TOKEN -> new RefreshToken(),
    OAuthGrantType.PASSWORD -> new Password()
  )
}