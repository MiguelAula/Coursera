import patmat.CodeTree
import patmat.Huffman._

val text: List[Char] = string2Chars("Hello")
val codeTree: CodeTree = createCodeTree(text)
val encoded: List[Bit] = encode(codeTree)(text)
val codeTable: CodeTable = convert(codeTree)
val fastEncoded: List[Bit] = quickEncode(codeTree)(text)
val decoded: List[Char] = decode(codeTree,encoded)
