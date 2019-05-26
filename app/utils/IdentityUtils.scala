package utils

import java.util.UUID

import org.bson.types.ObjectId

import scala.util.Random

object IdentityUtils {
  private val random = Random
  private val minOTP = Math.pow(10, 5).asInstanceOf[Int]
  private val maxOTP = Math.pow(10, 6).asInstanceOf[Int] - 1
  def generateSequentialUniqueId(): String = new ObjectId().toString

  def generateRandomUniqueId(): String = UUID.randomUUID.toString.replace("-", "")

  def generateOTP(): String = (random.nextInt(maxOTP - minOTP + 1) + minOTP).toString
}
