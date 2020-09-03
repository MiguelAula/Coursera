object week2 extends App {
  import scala.annotation.tailrec
  //WEEK2 EXERCISE

  //2.1
  type FunSet = Int => Boolean
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /*
  Define a function which creates a singleton set from one integer value: the set represents the set of the one given element.
  Its signature is as follows:
   */
  def singletonSet(elem: Int): FunSet = (x: Int) => x == elem

  /*
  Define the functions union,intersect, and diff, which takes two sets, and return, respectively, their union, intersection and differences.
  diff(s, t) returns a set which contains all the elements of the set s that are not in the set t.
  These functions have the following signatures:
   */
  def union(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) || t(x)
  def intersect(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && t(x)
  def diff(s: FunSet, t: FunSet): FunSet = (x: Int) => s(x) && !t(x) || !s(x) && t(x)

  /*
  Define the function filter which selects only the elements of a set that are accepted by a given predicate p.
  The filtered elements are returned as a new set. The signature of filter is as follows:
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = (x: Int) => s(x) && p(x)


  //2.2
  /*
  In this part, we are interested in functions used to make requests on elements of a set.
  The first function tests whether a given predicate is true for all elements of the set.
  This forall function has the following signature:

  def forall(s: FunSet, p: Int => Boolean): Boolean
   */

  /*
  Implement forall using linear recursion. For this, use a helper function nested in forall.
  Its structure is as follows (replace the ???):
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true
      else if (s(a) && !p(a)) false
      else iter(a+1)
    }
    iter(-1000)
  }

  /*
  Using forall, implement a function exists which tests whether a set contains at least one element for which the given predicate is true.
  Note that the functions forall and exists behave like the universal and existential quantifiers of first-order logic.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean =
    !forall(s,(x: Int) => !p(x))

  /*
  Finally, using forall or exists, write a function map which transforms a given set into another one by applying to each of its elements the given function.
  map has the following signature:
   */
  def map(s: FunSet, f: Int => Int): FunSet = {
    y => exists(s, x => f(x) == y)
  }


}
