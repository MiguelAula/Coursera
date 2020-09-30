import forcomp.Anagrams.{combinations, dictionaryByOccurrences, sentenceAnagrams, subtract, wordOccurrences}

val a = for {
  i <- List(1,2,3,4,5)
  j <- List(List(1,2,3,4,5))
} yield (i,j)

val b = (1 to 5).map(i => i)

val oc1 = wordOccurrences("hello")
val oc2 = wordOccurrences("h")
val sub = subtract(oc1,oc2)

val ss = if (dictionaryByOccurrences.contains(oc1)) dictionaryByOccurrences(oc1)
//val s = sentenceAnagrams(List("Yes", "man"))