import org.scalatest.FunSuite
import week2._

class Week2Test extends FunSuite {
  test("week2") {
    val set1 = (x: Int) => x >= -5 && x <= 5
    val set2 = (x: Int) => x >= 0 && x <= 10

    val union_set = union(set1,set2)
    assert(union_set(-5) && !union_set(-6) && union_set(10) && !union_set(11))

    val intersect_set = intersect(set1,set2)
    assert(intersect_set(0) && !intersect_set(-1) && intersect_set(5) && !intersect_set(6))

    val diff_set = diff(set1,set2)
    assert(diff_set(-1) && !diff_set(0) && diff_set(9) && !diff_set(3))

    val filterPositive = filter(set1,(x) => x >= 0)
    assert(filterPositive(3) && !filterPositive(-1))

    assert(forall(union_set,(x) => x >= -5 && x <= 10))
    assert(!exists(set1,(x) => x < -5))

    val mapPlus5 = map(set1,(x) => x+5)
    assert(forall(mapPlus5,set2))

    val mapPlus4 = map(set1,(x) => x+4)
    assert(!forall(mapPlus4,set2))
  }
}
