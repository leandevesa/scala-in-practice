sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x,xs) => x + sum(xs)
}

def append[A](a1: List[A], a2: List[A]): List[A] =
  a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

// 3.1
val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + sum(t)
  case _ => 101
}

// 3.2
def tail[A](list: List[A]): List[A] = list match {
  case Nil => Nil
  case Cons(_,xs) => xs
}

tail(List(1,2,3,4,5))

// 3.3
def replaceHead[A](list: List[A], newHead: A): List[A] = list match {
  case Nil => Nil
  case Cons(_,xs) => Cons(newHead,xs)
}

replaceHead(List(1,2,3,4,5), 7)

// 3.4
def drop[A](l: List[A], n: Int): List[A] = n match {
  case 0 => l
  case _ => drop(tail(l), n-1)
}

drop(List(1,2,3,4,5), 4)

// 3.5
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(x,xs) => if(f(x)) dropWhile(xs, f) else Cons(x,xs)
}

def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Cons(h,t) if f(h) => dropWhile2(t, f)
  case _ => l
}

def x[A](x:A):Boolean = x==3
val ex1 = dropWhile(List(1,2,3,4,5), (x: Int) => x < 4)
val ex2 = dropWhile2(List(1,2,3,4,5), (x: Int) => x < 4)


// Scala can infer this fact if we group dropWhile into 2 argument
// Here dropWhile is curried
def dropWhile3[A](as: List[A])(f: A => Boolean): List[A] =
  as match {
    case Cons(h,t) if f(h) => dropWhile3(t)(f)
    case _ => as
  }

val xs: List[Int] = List(1,2,3,4,5)
val ex3 = dropWhile3(xs)(x => x < 4)

// 3.6
def init[A](l: List[A]): List[A] = l match {
  case Nil => sys.error("init of empty list")
  case Cons(_,Nil) => Nil
  case Cons(h,t) => Cons(h,init(t))
}

//3.9



def length[A](l: List[A]): Int =
  foldRight(l, 0)((_,nombreQueQuieras) => nombreQueQuieras+1)

length(xs)

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

//3.12
def reverse[A](l:List[A]): List[A] = {
  def loop[A](l:List[A], revAcc: List[A]): List[A] = {
    l match {
      case Nil => revAcc
      case Cons(x,xs) => loop(xs, Cons(x, revAcc))
    }
  }

  loop(l, List())
}

reverse(xs)

def reverse2[A](l:List[A]): List[A] =
  foldLeft(l, List[A]())((revAcc,x)=> Cons(x, revAcc))

//3.14
def appendLists[A](l:List[A], l2:List[A]): List[A] =
  foldRight(l, l2)((head,santi)=> Cons(head,santi))

//3.15
def flat[A](l:List[List[A]]): List[A] =
  foldRight(l, List[A]())(appendLists)

//3.18
def map[A,B](as: List[A])(f: A => B): List[B] =
  foldRight(as, List[B]())((x,algo) => Cons(f(x),algo))

map(xs)(_*2)

def flatmap[A,B](as: List[A])(f: A => List[B]): List[B] =
  foldRight(as, List[B]())((x,algo) =>
    appendLists(algo, f(x)))

def flatmap2[A,B](as: List[A])(f: A => List[B]): List[B] =
  flat(map(as)(f))

flatmap(xs)((x) => List(x+x))