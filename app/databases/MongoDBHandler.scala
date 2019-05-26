package databases

import java.time.Instant

import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.IndexOptions
import org.mongodb.scala.result.UpdateResult
import org.mongodb.scala.{Completed, MongoClient, MongoCollection}
import play.api.Logger

import scala.concurrent.{ExecutionContext, Future}

object MongoDBHandler {
  private val mongoClient = MongoClient()
  private val database = mongoClient.getDatabase("cashm-auth-db")

  def close(): Unit = {
    Logger.info("[MongoDBHandler] Closing MongoDB")
    mongoClient.close()
  }

  def insertOne(collectionName: String, document: Document)(implicit ec: ExecutionContext): Future[Completed] = {
    val collection = getCollection(collectionName)
    val doc = addCreateUpdateTime(document)

    collection.insertOne(doc).toFuture
      .recoverWith { case e: Throwable =>
        Logger.info(s"[MonogDBError]insertOne failed and recover with exception ${e.toString}", e)
        Future.failed(e)
      }
  }

  def updateOne(collectionName: String, checkDocument: Document, updateDocument: Document)(implicit ec: ExecutionContext): Future[UpdateResult] = {
    val collection = getCollection(collectionName)

    var doc = updateDocument.copy()
    val now = Instant.now.getEpochSecond
    doc += "updateTime" -> now

    collection.updateOne(checkDocument, Document("$set" -> doc)).toFuture
      .recoverWith { case e: Throwable =>
        Logger.info(s"[MonogDBError]updateOne failed and recover with exception ${e.toString}", e)
        Future.failed(e)
      }
  }

  private def addCreateUpdateTime(document: Document): Document = {
    var doc = document.copy()
    val now = Instant.now.getEpochSecond
    doc += "creationTime" -> now
    doc += "updateTime" -> now

    doc
  }

  def getCollection(collectionName: String): MongoCollection[Document] = {
    Logger.info(s"Mongo database getting collection $collectionName from ${database.name}")
    val mongoCollection = database.getCollection(collectionName)
    Logger.info(s"Mongo database got collection $collectionName as $mongoCollection")
    mongoCollection
  }

  def find(collectionName: String, document: Document)(implicit ec: ExecutionContext): Future[Seq[Document]] = {
    val collection = getCollection(collectionName)

    collection.find(document).sort(Document("creationTime" -> -1, "updateTime" -> -1))
      .toFuture()
      .recoverWith { case e: Throwable =>
        Logger.info(s"[MonogDBError]find-1 failed and recover with exception ${e.toString}", e)
        Future.failed(e)
      }
  }

  def find(collectionName: String, document: Document, size: Int, page: Int)(implicit ec: ExecutionContext): Future[Seq[Document]] = {
    val collection = getCollection(collectionName)

    collection.find(document).sort(Document("creationTime" -> -1, "updateTime" -> -1)).skip(size * (page - 1)).limit(size)
      .toFuture()
      .recoverWith { case e: Throwable =>
        Logger.info(s"[MonogDBError]find-2 failed and recover with exception ${e.toString}", e)
        Future.failed(e)
      }
  }

  def count(collectionName: String, document: Document)(implicit ec: ExecutionContext): Future[Long] = {
    val collection = getCollection(collectionName)

    collection.count(document).toFuture
      .recoverWith { case e: Throwable =>
        Logger.info(s"[MonogDBError]count failed and recover with exception ${e.toString}", e)
        Future.failed(e)
      }
  }

  def createIndex(collectionName: String, document: Document, isUnique: Boolean = false)(implicit ec: ExecutionContext): Unit = {
    Future {
      getCollection(collectionName)
        .createIndex(document, new IndexOptions().unique(isUnique)).toFuture()
        .map { doc => println(s"Index status for $collectionName is $doc")
        }
    }
  }
}