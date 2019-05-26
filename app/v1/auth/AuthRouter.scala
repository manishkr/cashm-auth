package v1.auth

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class AuthRouter @Inject()(controller: AuthController) extends SimpleRouter {
  val prefix = "/v1/auth"

  override def routes: Routes = {
    case GET(p"/") => controller.index
  }
}
