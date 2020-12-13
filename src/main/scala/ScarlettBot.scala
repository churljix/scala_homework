import scala.io.Source

object ScarlettBot extends App {
  val botToken = "1425522412:AAEFhMNcaFaRq6iHWjcgUbzks4Njxb1Gi9U"
  val webSite = s"https://api.telegram.org/bot$botToken"

  // procedures
  val getUpdates = "/getUpdates"

  val update = Source.fromURL(webSite + getUpdates)

  print(update.mkString)
}



