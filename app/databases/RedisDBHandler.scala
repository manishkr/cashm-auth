package databases

import akka.actor.ActorSystem
import akka.util.Timeout
import com.redis._
import com.redis.serialization.{Format, Stringified}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}


object RedisDBHandler{
  implicit val system: ActorSystem = ActorSystem("redis-client")
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  implicit val timeout : Timeout = 5 seconds

  private val redisClient = RedisClient("localhost", 6379)

  def set(collectionName : String, key : String, value : Stringified): Future[Boolean] = redisClient.set(getCompositeKey(collectionName, key), value)

  def set(collectionName : String, key : String, value : String): Future[Boolean] = redisClient.set(getCompositeKey(collectionName, key), value)

  def setIfNot(collectionName : String, key : String, value : String): Future[Boolean] = redisClient.setnx(getCompositeKey(collectionName, key), value)

  def get[A](collectionName : String, key : String)(implicit format : Format[A]): Future[Option[A]] = redisClient.get[A](getCompositeKey(collectionName, key))

  def setex(collectionName : String, key : String, value : String, expireTime : Int): Future[Boolean] = {
    redisClient.setex(getCompositeKey(collectionName, key), expireTime, value)
  }

  def delete(collectionName : String, key : String) = redisClient.del(Seq(getCompositeKey(collectionName, key)))

  def getAllKeysIn(collectionName: String) = redisClient.keys(collectionName + "*")

  private def getCompositeKey(collectionName : String, key : String) = s"$collectionName:$key"
}