val str = "Hello World"
val str2 = "Hi"
val str_filter: String = str filter (x => x.isUpper)

val list = List("A","2","3","4")
val a = (str foldLeft "")(_ + _)

def isPrime(n: Int): Boolean = !((2 to n-1) exists (x => n%x == 0))

isPrime(3)
isPrime(5)
isPrime(7)
isPrime(10)
isPrime(21)
isPrime(23)

var scanres = list.scanLeft(Map())((m,s) => m + (s -> 1))
println(scanres)