package fpinscala.parsing

import language.higherKinds
import scala.util.matching.Regex

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
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]):Parser[List[A]]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  implicit def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  implicit def string(s: String):Parser[String]
  implicit def int(i: Int):Parser[Int]
  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]
  def listOfN[A](n: Int, p: Parser[A]):Parser[List[A]]
  def many[A](p: Parser[A]):Parser[List[A]]
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_)
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def fail[A](a: A): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(f andThen succeed)
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]):Parser[B]
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = p1.flatMap(x => p2.map(y => (x,y))) // = map2(p1, p2)((_, _))
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = p1.flatMap(x => p2.map(y => f(x,y))) //map(product(p1, p2))(f.tupled)


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]):Parser[B] = self.flatMap(p)(f)
    def many :Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def listOfN(n: Int) = self.listOfN(n, p)

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

    def productLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
      ???
      // product(success, success) == Right
      // product(fail, success) == product(fail, fail) == Left
      // product(success, fail) == Left
    }

  }

  val numA:Parser[Int] = char('a').many.slice.map(_.length)
  def zeroOrMore(c: Char) = char(c).many.slice.map(_.length)
  def oneFollowedBy(c: Char, c2: Char) = zeroOrMore(c) ** char(c2).many1.slice.map(_.length)
  def nFollowedByNChar(c: Char):Parser[List[Char]] = "0-9".r.flatMap(x => char(c).listOfN(x.toInt))

  def jsonParser[Err, Parser[+_]](P: Parsers3[Err, Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
  }
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
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