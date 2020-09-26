package scalaz_experiments.reader_monad

import cats._
import cats.data.Reader
import cats.implicits._

//translated from https://engineering.dollarshaveclub.com/the-reader-monad-example-motivation-542c54ccfaa8

object WithoutReaderMonad {
  type HTML = String

  private def div(children: List[HTML]): HTML =
    "<div>" + children.mkString("") + "</div>"

  private def h1(children: List[HTML]): HTML =
    "<h1>" + children.mkString("") + "</h1>"

  private def p(children: List[HTML]): HTML =
    "<p>" + children.mkString("") + "</p>"

  def view(email: String) = div(List(page(email)))

  private def page(email: String) =
    div(List(
      topNav(),
      content(email)
    ))

  private def topNav(): HTML =
    h1(
      List("OurSite.com")
    )

  private def content(email: String): HTML =
    div(List(
      h1(List("Custom content for " + email)),
      left(),
      right(email)
    ))

  private def left(): HTML =
    p(List("This is the left side"))

  private def right(email: String) =
    div(List(article(email)))

  private def article(email: String) =
    div(List(
      p(List("This is an article")),
      widget(email)
    ))

  private def widget(email: String) =
    div(List(p(List("Hey " + email + ", we've got a great offer for you!"))))

  def main(args: Array[String]): Unit = {
    val result = view("leopold.niklas@gmail.com")

    assert(result == "<div><div><h1>OurSite.com</h1><div><h1>Custom content for leopold.niklas@gmail.com</h1><p>This is the left side</p><div><div><p>This is an article</p><div><p>Hey leopold.niklas@gmail.com, we've got a great offer for you!</p></div></div></div></div></div></div>")
  }

}


object ReaderMonadUsage {
  type HTML = String

  case class Context(email: String)

  private def div(children: List[HTML]): HTML =
    "<div>" + children.mkString("") + "</div>"

  private def h1(children: List[HTML]): HTML =
    "<h1>" + children.mkString("") + "</h1>"

  private def p(children: List[HTML]): HTML =
    "<p>" + children.mkString("") + "</p>"

  def view: Reader[Context, HTML] = for (
    p <- page
  ) yield div(List(p))

  private def page = for (
    c <- content
  ) yield div(List(topNav, c))

  private def topNav: HTML =
    h1(List("OurSite.com"))

  private def content = for (
    context <- ask[Context];
    r <- right
  ) yield div(List(h1(List("Custom content for " + context.email)), left, r))

  private def ask[R]: Reader[R, R] = Reader(r => r)

  private def left: HTML =
    p(List("This is the left side"))

  private def right = for (
    a <- article
  ) yield div(List(a))

  private def article = for (
    w <- widget
  ) yield div(List(p(List("This is an article")), w))

  private def widget = for (
    context <- ask[Context]
  ) yield div(List(p(List("Hey " + context.email + ", we've got a great offer for you!"))))

  def main(args: Array[String]): Unit = {
    val widgetReader: Reader[Context, HTML] = widget;
    val widgetResult = widgetReader.run(Context("leopold.niklas@gmail.com"))
    assert(widgetResult == "<div><p>Hey leopold.niklas@gmail.com, we've got a great offer for you!</p></div>")

    val result = view.run(Context("leopold.niklas@gmail.com"))
    assert(result == "<div><div><h1>OurSite.com</h1><div><h1>Custom content for leopold.niklas@gmail.com</h1><p>This is the left side</p><div><div><p>This is an article</p><div><p>Hey leopold.niklas@gmail.com, we've got a great offer for you!</p></div></div></div></div></div></div>")
  }

}
