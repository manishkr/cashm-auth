package v1.user

import datamodel._
import datamodel.security.oauth._
import javax.inject.Inject
import play.api.Logger
import play.api.data.Form
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.mvc._
import scalaoauth2.provider.AuthInfo
import v1.auth.{AuthBaseController, AuthControllerComponents, AuthRequest}

import scala.concurrent.{ExecutionContext, Future}

class UserController @Inject()(cc: AuthControllerComponents)(dataHandler: OAuthDataHandler)(ws: WSClient)(implicit ec: ExecutionContext)
  extends AuthBaseController(cc) with OAuth2Provider {

  override val tokenEndpoint = new OAuthTokenEndpoint

  private val logger = Logger(getClass)

  private val form: Form[User] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "mobile" -> nonEmptyText,
        "password" -> nonEmptyText
      )(User.apply)(User.unapply)
    )
  }

  private val changePasswordForm: Form[ChangePassword] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "old_password" -> nonEmptyText,
        "new_password" -> nonEmptyText
      )(ChangePassword.apply)(ChangePassword.unapply)
    )
  }

  private val resetPasswordForm: Form[ResetPassword] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "mobile" -> nonEmptyText,
        "otp" -> nonEmptyText,
        "password" -> nonEmptyText
      )(ResetPassword.apply)(ResetPassword.unapply)
    )
  }

  private val mobileVerificationForm: Form[MobileVerification] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "mobile" -> nonEmptyText,
        "otp" -> nonEmptyText
      )(MobileVerification.apply)(MobileVerification.unapply)
    )
  }

  private val userProfileForm: Form[UserProfile] = {
    import play.api.data.Forms._

    Form(
      mapping(
        "first_name" -> nonEmptyText,
        "last_name" -> nonEmptyText,
        "email" -> nonEmptyText
      )(UserProfile.apply)(UserProfile.unapply)
    )
  }

  def process: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("process: ")
    processJsonPost()
  }

  def login: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("login: ")
    processLogin()
  }

  def changePassword: Action[AnyContent] = AuthAction.async { implicit request =>
    authorize(dataHandler) { authInfo =>
      logger.trace("changePassword: ")
      processChangePassword(authInfo)
    }
  }

  def resetPassword: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("resetPassword: ")
    processResetPassword()
  }

  def createProfile: Action[AnyContent] = AuthAction.async { implicit request =>
    authorize(dataHandler) { authInfo =>
      logger.trace("resetPassword: ")
      processUserProfileCreation(authInfo)
    }
  }

  def verifyMobile: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("verifyMobile: ")
    processMobileVerification()
  }

  def forgotPassword(mobile: String): Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("forgotPassword: ")
    User.forgotPassword(mobile, ws).map {
      case true => Ok(Json.obj("status" -> "Sent you OTP on your mobile"))
      case false => BadRequest(Json.obj("status" -> "No user exist with such mobile"))
    }
  }

  def resendOTP(mobile: String): Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("resendOTP: ")
    User.resendOTP(mobile, ws).map {
      case true => Ok(Json.obj("status" -> "Sent you OTP on your mobile"))
      case false => BadRequest(Json.obj("status" -> "No user exist with such mobile"))
    }
  }

  def getUserId(mobile: String): Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("getUserId: ")
    request.headers.get("auth_token") match {
      case Some(_) => getUser(mobile)
      case _ => authorize(dataHandler) { _ =>
        User.findUserId(mobile) map {
          case Some(userId) => Ok(Json.obj("user_id" -> userId))
          case _ => NotFound
        }
      }
    }
  }

  private def getUser[A](mobile: String)(implicit request: AuthRequest[A])= {
   val result: Option[Future[Result]] = for {
      authToken <- request.headers.get("auth_token")
      authSecret <- request.headers.get("auth_secret")
    } yield {
      val oAuthPair = OAuthPair(authToken, authSecret)
      for {
        authUser <- OAuthPair.getUser(oAuthPair)
        user <- User.findUserId(mobile)
      } yield {
        user match{
          case Some(userId) =>  Ok(Json.obj("user_id" -> userId))
          case _ => Unauthorized
        }
      }
    }

    result match {
      case Some(res) => res
      case _ => Future(BadRequest(Json.obj("status" -> "mobile_not_registered", "message" -> "Mobile is not registered with Kuber")))
    }
  }

  def isUserExist(mobile: String): Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("isUserExist: ")
    for{
      user <- User.findUserId(mobile)
      verified <- MobileVerification.isVerified(mobile)
    }yield {
      user match {
        case Some(_) => Ok(Json.obj("is_exist" -> true, "is_verified" -> verified))
        case _ => Ok(Json.obj("is_exist" -> false, "is_verified" -> false))
      }
    }
  }

  def sendMobileVerificationOTP(mobile: String): Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("sendMobileVerificationOTP: ")
    User.findUserId(mobile)
      .map {
        case Some(_) => {
          MobileVerification.generateAndSendOTP(mobile, ws).map {
            case true => Ok(Json.obj("status" -> "success"))
            case false => Ok(Json.obj("status" -> "alread_verified"))
          }
        }
        case _ => Future(NotFound)
      }.flatten
  }

  def resendMobileVerificationOTP(mobile: String): Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("resendMobileVerificationOTP: ")
    User.findUserId(mobile)
      .map {
        case Some(_) => {
          MobileVerification.resendOTP(mobile, ws).map {
            case true => Ok(Json.obj("status" -> "success"))
            case false => Ok(Json.obj("status" -> "alread_verified"))
          }
        }
        case _ => Future {
          NotFound
        }
      }.flatten
  }

  def getProfile: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("getProfile: ")
    authorize(dataHandler) { data =>
      UserProfile.get(data.user.userId).map {
        case Some(json) => Ok(Json.obj("profile" -> json))
        case _ => NotFound
      }
    }
  }

  def logout: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("logout: ")

    authorize(dataHandler) { data =>
      dataHandler.deleteAuthKey(data)
      Future(Ok(Json.obj("status" -> "success")))
    }
  }

  def getTotalCount: Action[AnyContent] = AuthAction.async { implicit request =>
    logger.trace("getTotalCount: ")

    authorize(dataHandler) { _ =>
      User.totalCount()
        .map { total =>
          Ok(Json.obj("total_user" -> total))
        }
    }
  }

  private def processJsonPost[A]()(implicit request: AuthRequest[A]): Future[Result] = {
    def failure(badForm: Form[User]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(user: User) = {
      user.save().map {
        case true => {
          Future(MobileVerification.generateAndSendOTP(user.mobile, ws))
          Future(issueAccessToken(dataHandler))
        }
        case false => Future {
          MobileVerification.isVerified(user.mobile).map { status =>
            Future{ if(!status){ MobileVerification.generateAndSendOTP(user.mobile, ws)}}
            Conflict(Json.obj("is_verified" -> status))
          }
        }
      }
    }.flatten.flatten

    form.bindFromRequest().fold(failure, success)
  }

  private def processLogin[A]()(implicit request: AuthRequest[A]): Future[Result] = issueAccessToken(dataHandler)

  private def processChangePassword[A](authInfo : AuthInfo[AccountInfo])(implicit request: AuthRequest[A]): Future[Result] = {
    def failure(badForm: Form[ChangePassword]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(changePassword: ChangePassword) = {
      changePassword.save(authInfo.user.userId).map {
        case true => Ok(Json.obj("status" -> "success"))
        case false => BadRequest(Json.obj("error" -> "password mismatch"))
      }
    }

    changePasswordForm.bindFromRequest().fold(failure, success)
  }

  private def processResetPassword[A]()(implicit request: AuthRequest[A]): Future[Result] = {
    def failure(badForm: Form[ResetPassword]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(resetPassword: ResetPassword) = {
      resetPassword.save().map {
        case true => issueAccessToken(dataHandler)
        case false => Future(BadRequest(Json.obj("error" -> "invalid otp")))
      }.flatten
    }

    resetPasswordForm.bindFromRequest().fold(failure, success)
  }

  private def processMobileVerification[A]()(implicit request: AuthRequest[A]): Future[Result] = {
    def failure(badForm: Form[MobileVerification]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(mobileVerification: MobileVerification) = {
      mobileVerification.verify().map {
        case true => Ok(Json.obj("status" -> "success"))
        case false => BadRequest(Json.obj("error" -> "invalid otp"))
      }
    }

    mobileVerificationForm.bindFromRequest().fold(failure, success)
  }

  private def processUserProfileCreation[A](authInfo : AuthInfo[AccountInfo])(implicit request: AuthRequest[A]): Future[Result] = {
    def failure(badForm: Form[UserProfile]) = {
      Future.successful(BadRequest(badForm.errorsAsJson))
    }

    def success(userProfileForm: UserProfile) = {
      userProfileForm.save(authInfo.user.userId).map {
        case true => Ok(Json.obj("status" -> "success"))
        case false => NotFound(Json.obj("error" -> "Not found"))
      }
    }

    userProfileForm.bindFromRequest().fold(failure, success)
  }
}
