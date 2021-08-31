
object Main extends {

  def main(args: Array[String]): Unit = {
    substitutionSed(text, commandStr)
  }

  var text = "unix is great os. unix is opensource. unix is free os.\n" +
    "learn operating system unix.\n" +
    "unix linux which one you choose.\n" +
    "unix is easy to learn. unix is a multiuser os.Learn unix .unix is a powerful."

  val commandStr: String = "s/unix/linux/2g"

  def substitutionSed(text: String, commandStr: String) :Unit = {
  val spliced = commandStr.split("/")
  val startsWithNumber = "([0-9]+[a-z]*)".r
  val justDigits= """([0-9]+)""".r

  spliced(0) match {
    case "s" => {
      if (spliced.size > 3){
        spliced(3) match {
          case "g" => {
            val result = separateLines(text)
              .map(x => x.replaceAll(spliced(1), spliced(2)))
            result.foreach(println)
          }
          case justDigits(value) => {
            separateLines(text).map(x =>
              replaceNthAppearance(
                str=x,
                toFind=spliced(1),
                toReplace=spliced(2),
                n= value.toInt)
            ).foreach(println)
          }
          case startsWithNumber(value) => {
            separateLines(text).map(x =>
              replaceAllNthAppearance(
                str=x,
                toFind=spliced(1),
                toReplace=spliced(2),
                n= getDigits(value))
            ).foreach(println)
          }
        }
      }else{
        val result = separateLines(text)
          .map(x => x.replaceFirst(spliced(1), spliced(2)))
        result.foreach(println)
      }
    }
    case _ =>
  }
  }

  def separateLines(str: String): Array[String] = {
    str.split("\\n")
  }

  def replaceNthAppearance(str: String, toFind: String,
                           toReplace: String, count: Int = 1, n: Int, acc: String = ""): String = {
    if(count < n) {
      val x = str.indexOf(toFind)
      val index = x + toFind.length
      replaceNthAppearance(str.substring(index, str.length), toFind,
        toReplace, count + 1, n, acc + str.substring(0, index))
    }else{
      val strReplaced = str.replaceFirst(toFind, toReplace)
      acc + strReplaced
    }
  }

  def replaceAllNthAppearance(str: String, toFind: String,
                              toReplace: String, count: Int = 1, n: Int, acc: String = ""): String = {
    if(count < n) {
      val x = str.indexOf(toFind)
      val index = x + toFind.length
      replaceAllNthAppearance(str.substring(index, str.length), toFind,
        toReplace, count + 1, n, acc + str.substring(0, index))
    }else{
      val strReplaced = str.replaceAll(toFind, toReplace)
      acc + strReplaced
    }
  }

  def getDigits(str: String, acc: String = ""): Int = {
    if(str(0).toString.matches("([0-9]+)")) {
      getDigits(str.substring(1, str.length),  acc + str(0))
    }else acc.toInt
  }
}