package v1.merchant

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class MerchantRouter @Inject()(controller: MerchantController) extends SimpleRouter {
  val prefix = "/v1/merchant_auth"

  override def routes: Routes = {
    case GET(p"/token") => controller.authPair
    case GET(p"/token_user") => controller.tokenUser
    case GET(p"/user_auth") => controller.userAuth
    case GET(p"/") => controller.index
  }
}
