package fpinscala.parsing





import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A]) {

  }
}

trait Parsers2[ParseError, Parser[+_]] {


  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def string(s: String):Parser[String]
  def or[A](a1: A, a2: A):Parser[String]
}

trait Parsers3[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def many[A](p: Parser[A]):Parser[List[A]]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  implicit def string(s: String):Parser[String]
  implicit def int(i: Int):Parser[Int]
  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]):Parser[List[A]]
  def many[A](P: Parser[A]):Parser[List[A]]
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]):Parser[B]


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]):Parser[B] = self.flatMap(p)(f)
    def many :Parser[List[A]] = self.many(p)
  }

  object Laws {
    import fpinscala.testing.Gen
    import fpinscala.testing.Prop
    import fpinscala.testing.Prop._
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    }
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
      equal(p, p.map(a => a))(in)
    }
    def untiLaw[A](in: Gen[String]): Prop = {
      forAll(in)(s => run(succeed(s))("") == Right(s))
    }

  }

  val numA:Parser[Int] = char('a').many.map(_.size)
  def zeroOrMore(c: Char) = char(c).many.map(_.size)
}

case class ZeroOrMore[Int](c: Char) {
  def parse(s: String): Int = ???
}



case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}