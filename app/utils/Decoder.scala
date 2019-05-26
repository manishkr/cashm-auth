package utils

object Decoder {
  def decodeBase64(bytesEncoded : String) = new String(java.util.Base64.getDecoder.decode(bytesEncoded))
}
