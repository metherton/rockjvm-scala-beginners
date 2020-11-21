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


  // higher order functions
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(test: A => Boolean): MyList[A]



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
  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(test: Nothing => Boolean): MyList[Nothing] = Empty

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
  def map[B](transformer: A => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))
  /*
      [1, 2].flatMap(n => [n, n +1]
      = [1, 2] ++ [2].flatMap(n => [n, n + 1])
      = [1, 2) ++ [2,3] ++ Empty.flatMap(n => [n, n + 1])
      = [1, 2) ++ [2,3] ++ Empty
      = [1,2,2,3]


   */
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

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
  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

}


object ListTest extends App {

  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val cloneListOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = Cons(4, Cons(5, Empty))
  val listOfStrings: MyList[String] = Cons("one", Cons("two", Cons("three", Empty)))

  println(anotherListOfIntegers.toString)
  println(listOfStrings.toString)
  println(listOfIntegers ++ anotherListOfIntegers)

  println(anotherListOfIntegers.map(elem => elem * 2))
  println(anotherListOfIntegers.map(_ * 2))

  println(anotherListOfIntegers.filter(_ % 2 == 0))

  println(listOfIntegers.flatMap((elem: Int) => Cons(elem, Cons(elem + 1, Empty))))

  println(cloneListOfIntegers == listOfIntegers)

}