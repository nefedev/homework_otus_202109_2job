case class Country(name:String, capital: String, area: Double)

object Wrapper {

  import io.circe._
  import io.circe.parser._
  import io.circe.JsonObject
  import io.circe.syntax._
  import java.io.{BufferedWriter, File, FileWriter}
  import scala.collection.mutable.ArrayBuffer
  import scala.io.Source.fromURL
  import io.circe.generic.auto._

  def main(args: Array[String]): Unit = {

    val json = parse((fromURL("https://raw.githubusercontent.com/mledoze/countries/master/countries.json").mkString)).getOrElse(null)

    val Right(docs) = json.as[List[JsonObject]]
    var tup: (String, String, Double) = ("","", 0.0)
    val arr = ArrayBuffer[(String, String, Double)]()

    docs.map(Json.fromJsonObject).map(_.noSpaces).foreach(it => {
      val cursor: HCursor = parse(it).getOrElse(null).hcursor
      val official: Decoder.Result[String] = cursor.downField("name").downField("official").as[String]
      val region: Decoder.Result[String] = cursor.downField("region").as[String]
      val area: Decoder.Result[Double] = cursor.downField("area").as[Double]
      val capital: Decoder.Result[String] = cursor.downField("capital").downArray.as[String]
      val cpt = capital.toSeq
      var cpt_name = ""

      if (region.getOrElse("").equals("Africa")) {
         if (cpt.size != 0) cpt_name = cpt(0)
         tup = (official.getOrElse(""), cpt_name, area.getOrElse(-1))
         arr += tup
      }
    })

    val resarr : ArrayBuffer[Country] = ArrayBuffer()

    arr.sortBy(_._3).reverse.take(10).foreach(it => {
      resarr += Country(it._1, it._2, it._3)
    })

    val file = new File(args(0))
    val bwrt = new BufferedWriter(new FileWriter(file))
    bwrt.write(resarr.asJson.toString())
    bwrt.close()
  }
}
