/*
005056008CD51EE889BB4660444A0462
051M08pL7kY9kqPWH4e4OW



297060000


00       50       56       00       8C       D5       1E       E8       89       BB       46       60       44       4A       04       62
00000000 01010000 01010110 00000000 10001100 11010101 00011110 11101000 10001001 10111011 01000110 01100000 01000100 01001010 00000100 01100010

00000000010100000101011000000000100011001101010100011110111010001000100110111011010001100110000001000100010010100000010001100010

 */


object Converter {
  @main
  def main(): Unit = {

    val guid16Str = "005056008CD51EE889BB4660444A0462"
    val guid22Str = guid16BinStrToGuid22Str(guid16Str)
    println("GUID 22: " + guid22Str)

    val guid22 = "01LT0AG27jgZro}V}}M7rW"
    val guid22Str16 = guid22To16hex(guid22Str)
    println("GUID 16: " + guid22Str16)
  }

  private def guid22To16hex(guid22Str : String ) = {

    val guid22Ints = guid22Str.map[Int]( x => {
      val ascii = x.toInt
      if(ascii >= '0' && ascii <= '9'){
        ascii - '0'
      }else if(ascii >= 'A' && ascii <= 'Z'){
        (ascii-55)
      }else if(ascii >= 'a' && ascii <= '{'){
        (ascii-61)
      }else{
        63
      }
    })

    val stringBinaryList = guid22Ints.map[String](x => {
      String.format("%6s", Integer.toString(x, 2)).replace(' ', '0')
    })
    val fullBinaryString = stringBinaryList.foldLeft(""){ _ + _ }.dropRight(4)
    val binaryList = fullBinaryString.foldLeft(List("")){
      (lst, char) => {
        if(lst.last.size < 8)
          lst.dropRight(1) :+ lst.last + char.toString
        else
          lst :+ char.toString
      }
    }
    val hexList = binaryList.map[Int](
      x => Integer.parseInt(x, 2)
    )
    hexList.foldLeft(""){
      (out, x) =>
        out + String.format("%2s", x.toHexString).replace(' ', '0')
    }
  }

  def guid16BinStrToGuid22Str(guid16Str: String) : String= {
    val guid16BinaryStr = guid16StringToBinaryString(guid16Str);

    val listOf6Str = guid16BinaryStr.foldLeft(List("")){
      (lst, char) => {
        if(lst.last.size < 6)
          lst.dropRight(1) :+ lst.last + char.toString
        else
          lst :+ char.toString
      }
    }
    val finalStringOf6 = listOf6Str.dropRight(1) :+ listOf6Str.last + "0000"
    finalStringOf6.map[String](
      x => {
        val intVal = Integer.parseInt(x, 2)

        if(intVal < 10){
          intVal.toString
        }else if(intVal <= 35){
          (intVal+55).toChar.toString
        }else if(intVal <= 62){
          (intVal+61).toChar.toString
        }else{
          "}"
        }
      }
    ).fold("")((x, y) => x + y)
  }


  def guid16StringToBinaryString(input: String): String ={
    val hexList = input.zipWithIndex.foldLeft(List.empty[Int]){
      (lst, zipChar) => {
        if(zipChar._2%2==0)
          lst :+ Integer.parseInt(zipChar._1.toString, 16) * 0x10
        else
          lst.dropRight(1) :+ lst.last + Integer.parseInt(zipChar._1.toString, 16)
      }
    }

    val stringBinaryList = hexList.map[String](x => {
      String.format("%8s", Integer.toString(x, 2)).replace(' ', '0')
    })

    stringBinaryList.foldLeft(""){ _ + _ }
  }

}