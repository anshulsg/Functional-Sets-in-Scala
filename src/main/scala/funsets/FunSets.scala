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
      def singleton(x: Int)(y: Int): Boolean ={
        x==y
      }
      singleton(elem)
    }
  

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
    def union(s: Set, t: Set): Set = {
      def func_union(x:Int): Boolean= s(x)||t(x)
      func_union
    }
  
  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
    def intersect(s: Set, t: Set): Set = {
      def func_intersection( x: Int): Boolean= s(x) && t(x)
      func_intersection
    }
  
  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = {
      def func_diff(x: Int): Boolean={
        if(s(x)&&t(x)) false
        else s(x)
      }
      func_diff
    }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = {
      intersect(s,p)
    }
  

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1001

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a==bound) true
      else if (s(a)&&(!p(a))) false
      else iter(a+1)
    }
    iter(-1000)
  }
  
  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = {
      def p_inv(x:Int): Boolean= !p(x)
      !forall(s,p_inv)
    }
  
  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = {
      def nill_f()(x: Int): Boolean= false

      def iter(a:Int, span: Set): Set={
        if(a==bound) span
        else{
          def temp ={
            if(s(a)) singletonSet(f(a))
            else nill_f()_
          }
          iter(a+1, union(span, temp))
        }
      }
      iter(-1000, nill_f()_)
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
