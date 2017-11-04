import java.io.File
import java.io.FileReader
package object parsers {

  case class **[+A, +B](a: A, b: B)
  
  sealed trait ParseResult[+Result]
  case class Success[+Result](res: Result, rest: Stream[Char]) extends ParseResult[Result]
  case class Failure(err: String) extends ParseResult[Nothing]
  
  trait Parser[+Result] { self =>
    def apply(input: Stream[Char]): ParseResult[Result]
    def map[NewResult](f: Result => NewResult): Parser[NewResult] = new Parser[NewResult] {
      def apply(input: Stream[Char]) = self(input) match {
        case Success(res, rest) => Success(f(res), rest)
        case Failure(err) => Failure(err)
      }
    }
    def flatMap[NewResult](f: Result => Parser[NewResult]): Parser[NewResult] = new Parser[NewResult] {
      def apply(input: Stream[Char]) = self(input) match {
        case Success(res, rest) => f(res)(rest)
        case Failure(err) => Failure(err)
      }
    }
    def replace[NewResult](result: NewResult) = map(_ => result)
    def withFilter(f: Result => Boolean) = for {
      x <- this
      _ <- if (f(x)) done(()) else fail("filter failed: " + x)
    } yield x
    def **[OtherResult](that: => Parser[OtherResult]) = for {
      x <- this
      y <- that
    } yield parsers.**(x, y)
    def ++[SuperResult >: Result](that: => Parser[SuperResult]): Parser[SuperResult] = new Parser[SuperResult] {
      lazy val other = that
      def apply(input: Stream[Char]) = self(input) match {
        case Success(res, rest) => Success(res, rest)
        case Failure(msg1) => other(input) match {
          case Success(res, rest) => Success(res, rest)
          case Failure(msg2) => Failure(msg1+msg2)
        }
      }
    }
    def *>[OtherResult](that: => Parser[OtherResult]) = this ** that map { case x ** y => y }
    def *<[OtherResult](that: => Parser[OtherResult]) = this ** that map { case x ** y => x }
    def opt = this.map(Some(_)) ++ done(None)
    def star: Parser[Vector[Result]] = for {
      head <- this.opt
      res <- head.map(x => star.map(xs => x +: xs)).getOrElse(done(Vector()))
    } yield res
    def plus: Parser[Vector[Result]] = for {
      head <- this
      tail <- star
    } yield head +: tail
    def sepSeq(sep: Parser[_]): Parser[Vector[Result]] =
      (this ** (sep *> this).star).opt.map {
      case None => Vector()
      case Some(head ** tail) => head +: tail
    }
  }
  
  def done[Result](x: Result) = new Parser[Result] {
    def apply(input: Stream[Char]) = Success(x, input)
  }
  def fail(msg: String) = new Parser[Nothing] {
    def apply(input: Stream[Char]) = Failure(msg)
  }
  
  val char = new Parser[Char] {
    def apply(input: Stream[Char]) = input match {
      case ch #:: rest => Success(ch, rest)
      case _ => Failure("eof")
    }
  }
  def lit(str: String) = {
    def litIx(ix: Int): Parser[String] =
      if (ix == str.length) done(str)
      else for {
        x <- char
        if x == str(ix)
        y <- litIx(ix+1)
      } yield y
    litIx(0)
  }
  def parse[Result](parser: Parser[Result], file: File) = {
    val input = new FileReader(file)
    def read(): Stream[Char] = {
      val ch = input.read()
      if (ch == -1) Stream.Empty
      else ch.asInstanceOf[Char] #:: read()
    }
    try {
      val chars = read()
      parser(chars) match {
        case Success(res, Stream.Empty) => res
        case Success(res, rest) => throw new Exception("unexpected eof\n"+rest + "\n" + res)
        case Failure(msg) => throw new Exception(msg)
      }
    }
    finally {
      input.close()
    }
  }

}