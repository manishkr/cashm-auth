package datamodel

import databases.MongoDBHandler
import org.mongodb.scala.bson.collection.immutable.Document
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.{ExecutionContext, Future}

case class UserProfile(firstName : String, lastName: String, email : String) {
  def save(userId : String)(implicit ec: ExecutionContext) ={
    MongoDBHandler.updateOne(UserProfile.collectionName, Document("userId" -> userId),
      Document("firstName" -> firstName, "lastName" -> lastName, "email" -> email))
      .map(_.getMatchedCount > 0)
      .map{
        case true => true
        case false => MongoDBHandler.insertOne(UserProfile.collectionName, Document("userId" -> userId,
          "firstName" -> firstName, "lastName" -> lastName, "email" -> email))
          true
      }
  }
}

object UserProfile{
  val collectionName = "UserProfile"
  def get(userId: String)(implicit ec: ExecutionContext): Future[Option[JsObject]] = {
    MongoDBHandler.find(collectionName, Document("userId" -> userId))
      .map(_.map(doc => jsonify(doc)))
      .map(_.headOption)
  }

  private def jsonify(doc : Document) = Json.obj("first_name" -> doc.getString("firstName"),
    "last_name" -> doc.getString("lastName"), "email" -> doc.getString("email"))
}