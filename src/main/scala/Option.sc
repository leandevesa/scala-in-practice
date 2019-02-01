sealed trait Option[+A] {

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(a)  => a
      case _        => default
    }





  // 4.1

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a)  => Some(f(a))
      case _        => None
    }


  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a)  => f(a)
      case _        => None
    }

  def map2[B,C](b: Option[B])(f: (A, B) => C): Option[C] =
    this.flatMap( x => {
      b.map( y => {
       f(x,y)
      })
    }
    )

  def filter[A](opt: Option[A])(f: A => Boolean): Option[A] =
    opt match {
      case Some(a) if f(a)  => opt
      case _                => None
    }

  // 4.3

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def sequence[B](b: List[Option[B]]): Option[List[B]] =
  b match {
    case Nil        => Some(Nil)
    case h :: t     => for {
      hh <- h
      st <- sequence(t)
    } yield hh :: st
  }

val a = Some(10)
val b = None


val c = List(Some(1), Some(2), Some(3))

sequence(c)