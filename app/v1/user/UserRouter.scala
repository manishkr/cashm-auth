package v1.user

import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class UserRouter @Inject()(controller: UserController) extends SimpleRouter {
  val prefix = "/v1/user"

  override def routes: Routes = {
    case POST(p"/register") => controller.process
    case POST(p"/login") => controller.login
    case POST(p"/change_password") => controller.changePassword
    case POST(p"/reset_password") => controller.resetPassword
    case POST(p"/verify_mobile") => controller.verifyMobile
    case POST(p"/profile") => controller.createProfile

    case GET(p"/forgot_password/$mobile") => controller.forgotPassword(mobile)
    case GET(p"/resend_otp/$mobile") => controller.resendOTP(mobile)
    case GET(p"/user_id/$mobile") => controller.getUserId(mobile)
    case GET(p"/check_user/$mobile") => controller.isUserExist(mobile)
    case GET(p"/verify_mobile/$mobile") => controller.sendMobileVerificationOTP(mobile)
    case GET(p"/verify_mobile_resend/$mobile") => controller.resendMobileVerificationOTP(mobile)
    case GET(p"/profile") => controller.getProfile
    case GET(p"/logout") => controller.logout

    case GET(p"/total_user") => controller.getTotalCount
  }
}