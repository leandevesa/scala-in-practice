sealed trait Either[+E, +A] {

  def map[J](f: A => J): Either[E,J] =
    this match {
      case Right(x) => Right(f(x))
      case Left(e)  => Left(e)
    }

  def flatMap[C>:E,J](f: A => Either[C,J]): Either[C,J] =
    this match {
      case Right(x) => f(x)
      case Left(e)  => Left(e)
    }

  def map2[C>:E,J,B](b: Either[C,J])(f: (A, J) => B): Either[C,B] =
    this.flatMap(x => b.map(y => f(x,y)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


val a = Right("mauri")
val b = Left("santi")
val c = Right("TRUMP!")

a.map2(b)((x, y) => x + " le vende limone a " + y)
a.map2(c)((x, y) => x + " le vende limone a " + y)