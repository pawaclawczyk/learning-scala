import patmat.Huffman._

val code = createCodeTree("Hello, World!".toList)
val encoded = encode(code)("dell, Ho".toList)
val decoded = decode(code, encoded)

decoded.mkString

val encodedWithQuick = quickEncode(code)("dell, Ho".toList)

encoded == encodedWithQuick

decode(code, encodedWithQuick).mkString
