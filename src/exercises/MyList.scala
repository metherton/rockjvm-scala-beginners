package exercises

abstract class MyList[+A] {

  /*
      head = first element of list
      tail = remainder of list
      isEmpty = is this list empty
      add = new list with this element added
      tostring = a string representation of the list
   */

  def head: A
  def tail: MyList[A]
  def add[B >: A](el: B): MyList[B]
  def isEmpty: Boolean

  def map[B](transformer: MyTransformer[A, B]): MyList[B]
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(test: MyPredicate[A]): MyList[A]
  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"

  // concatenation
  def ++[B >: A](list: MyList[B]): MyList[B]
}

// objects can extend classes
case object Empty extends MyList[Nothing] {

  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def add[B >: Nothing](el: B): MyList[B] = new Cons(el, Empty)
  def isEmpty: Boolean = true
  def map[B](transformer: MyTransformer[Nothing, B]): MyList[B] = Empty
  def flatMap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = Empty
  def filter(test: MyPredicate[Nothing]): MyList[Nothing] = Empty

  // concatenation
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

  def printElements: String = ""
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {

  def head: A = h
  def tail: MyList[A] = t
  def add[B >: A](el: B): MyList[B] = new Cons(el, this)
  def isEmpty: Boolean = false
  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  /*
      [1, 2, 3].map(n * 2)
        = new Cons(2, [2, 3].map(n * 2)
        = new Cons(2, new Cons(4, [3].map(n * 2)))
        = new Cons(2, new Cons(4, new Cons(6, Empty.map(n * 2))))
        = new Cons(2, new Cons(4, new Cons(6, Empty))))
   */
  def map[B](transformer: MyTransformer[A, B]): MyList[B] =
    new Cons(transformer.transform(h), t.map(transformer))
  /*
      [1, 2].flatMap(n => [n, n +1]
      = [1, 2] ++ [2].flatMap(n => [n, n + 1])
      = [1, 2) ++ [2,3] ++ Empty.flatMap(n => [n, n + 1])
      = [1, 2) ++ [2,3] ++ Empty
      = [1,2,2,3]


   */
  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)

  /*
      [1, 2] ++ [3, 4, 5]
      = new Cons(1, [2] ++ [3, 4, 5])
      = new Cons(1, new Cons(2, Empty ++ [3, 4, 5])))
      = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5)])))
   */
  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)
  /*
      [1, 2, 3].filter(n % 2 == 0) =
        [2, 3].filter(n % 2 == 0) =
        = new Cons(2, [3].filter(n % 2 == 0)
        = new Cons(2, Empty.filter(n % 2 == 0)
        = new Cons(2, Empty)
   */
  def filter(predicate: MyPredicate[A]): MyList[A] =
    if (predicate.test(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

}



trait MyPredicate[-T] {
  def test(elem: T): Boolean
}
trait MyTransformer[-A, B] {
  def transform(elem: A): B
}

object ListTest extends App {

  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val cloneListOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = Cons(4, Cons(5, Empty))
  val listOfStrings: MyList[String] = Cons("one", Cons("two", Cons("three", Empty)))

  println(anotherListOfIntegers.toString)
  println(listOfStrings.toString)
  println(listOfIntegers ++ anotherListOfIntegers)

  println(anotherListOfIntegers.map(new MyTransformer[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }))

  println(anotherListOfIntegers.filter(new MyPredicate[Int] {
    override def test(elem: Int): Boolean = elem % 2 == 0
  }))

  println(listOfIntegers.flatMap(new MyTransformer[Int, MyList[Int]] {
    override def transform(elem: Int): MyList[Int] = Cons(elem, Cons(elem + 1, Empty))
  }))

  println(cloneListOfIntegers == listOfIntegers)

}