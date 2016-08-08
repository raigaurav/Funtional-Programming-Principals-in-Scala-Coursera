package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = {
    val setFunc = (x: Int) => x == elem
    setFunc
  }


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = {
    val un = (x: Int) => (s(x) | t(x))
    un
  }

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = {
    val in = (x: Int) => s(x) & t(x)
    in
  }

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = {
    val df = (x: Int) => if(s(x)==true) (s(x) ^ t(x)) else false
    df
  }

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = {
    val fl = (x: Int) => s(x) & p(x)
    fl
  }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == -1000) true
      else if (!contains(filter(s,p),a) && contains(s,a)) false
      else iter(a-1)
    }
    iter(bound)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (contains(filter(s,p),a) && contains(s,a)) true
      else if (a == -1000) false
      else iter(a-1)
    }
    iter(bound)
  }
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {
    var tr = singletonSet(1001)
    def iter(a: Int): Int => Boolean = {
      if(a == -1000) diff(tr, singletonSet(1001))
      else if (contains(s,a)) {
        tr = union(tr,singletonSet(f(a)))
        iter(a-1)
      }
      else iter(a-1)
    }
    iter(bound)
  }
  
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
