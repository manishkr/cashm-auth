package datamodel

import datamodel.security.oauth.OAuthPair

import scala.concurrent.{ExecutionContext, Future}

case class MerchantAuth(userId : String)

object MerchantAuth {
  def get(merchantAuth: MerchantAuth)(implicit ec: ExecutionContext): Future[OAuthPair] = {
    OAuthPair.get(merchantAuth.userId).map {
      case Some(pair) => pair
      case _ => {
        val pair = OAuthPair.generate()
        pair.save(merchantAuth.userId)
        pair
      }
    }
  }
}