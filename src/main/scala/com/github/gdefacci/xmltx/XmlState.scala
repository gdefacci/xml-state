package com.github.gdefacci.xmlstate

class State[S, O](val run: S => (S, O)) {

  def map[B](f1: O => B): State[S, B] =
    new State(run.andThen { case (state, out) => state -> f1(out) })

  def flatMap[B](f1: O => State[S, B]): State[S, B] =
    new State[S, B](run.andThen { case (state, out) => f1(out).run(state) })

}

object XmlPredicate {
  def apply(p: xml.Elem => Boolean) = new XmlPredicate {
    def apply(el: xml.Elem): Boolean = p(el)
  }
}

trait XmlPredicate extends (xml.Elem => Boolean) { self =>

  def and(p: xml.Elem => Boolean) = XmlPredicate(el => self.apply(el) && p.apply(el))

  def or(p: xml.Elem => Boolean) = XmlPredicate(el => self.apply(el) || p.apply(el))

  def not = XmlPredicate(el => !self.apply(el))
}

object XmlState extends App {

  case class ByName(name: String) extends XmlPredicate {
    def apply(el: xml.Elem) = el.label == name
  }

  case class ByAttribute(name: String, predicate: String => Boolean) extends XmlPredicate {
    def apply(el: xml.Elem) = el.attribute(name).map(el => predicate(el.text)).getOrElse(false)
  }

  def read[T](f: xml.Elem => T) = new State[xml.Elem, T](s => s -> f(s))
  def write(s1: => xml.Elem) = new State[xml.Elem, Unit](s => (s1, (): Unit))
  def current = new State[xml.Elem, xml.Elem](s => (s, s))

  def attribute(name: String) = read[Option[String]] { nd =>
    nd.attribute(name).map(_.text)
  }

  def childElements(predicate: xml.Elem => Boolean) = read[Seq[xml.Elem]] { nd =>
    nd.child.collect {
      case el: xml.Elem if predicate(el) => el
    }
  }
  def childElementOpt(predicate: xml.Elem => Boolean): State[xml.Elem, Option[xml.Elem]] = childElements(predicate).map(_.headOption)
  def childElement(predicate: xml.Elem => Boolean): State[xml.Elem, xml.Elem] = childElements(predicate).map(_.head)

  def children(name: String) = childElements(_.label == name)
  def childOpt(name: String) = children(name).map(_.headOption)
  def child(name: String) = children(name).map(_.head)

  def setName(nm: String) = for {
    curr <- current
    _ <- write(curr.copy(label = nm))
  } yield ()

  def setAttr(nm: String, v: String) = for {
    curr <- current
    _ <- write(curr % xml.Attribute(None, nm, xml.Text(v), xml.Null))
  } yield ()

  def replace(el: xml.Elem, elem: xml.NodeSeq) = for {
    curr <- current
    _ <- write {
      curr.copy(child = curr.child.flatMap {
        case ch: xml.Elem if (ch == el) => elem
        case x => xml.NodeSeq.fromSeq(x)
      })
    }
  } yield ()

  def insertAfter(el: xml.Elem, newl: xml.NodeSeq) =
    replace(el, xml.NodeSeq.fromSeq(Seq(el)) ++ newl)

  def insertBefore(el: xml.Elem, newl: xml.Node) =
    replace(el, newl ++ xml.NodeSeq.fromSeq(Seq(el)))

  def updateAttr(nm: String, update: String => String) = for {
    curr <- current
    attr <- attribute(nm)
    _ <- attr match {
      case None => current
      case Some(v) => write(curr % xml.Attribute(None, nm, xml.Text(update(v)), xml.Null))
    }
  } yield ()

  def appendChild(ch: xml.NodeSeq) = for {
    curr <- current
    _ <- write(curr.copy(child = curr.child ++ ch))
  } yield ()

  def select[R](pred: xml.Elem => Boolean, st: State[xml.Elem, R]) =
    new State[xml.Elem, Seq[R]]({ curr =>
      val partChs: Seq[Either[xml.Node, (xml.Elem, R)]] = curr.child.collect {
        case ch: xml.Elem if pred(ch) => Right(st.run(ch))
        case ch => Left(ch)
      }
      val newChild = partChs.map {
        case Left(ch) => ch
        case Right(ch) => ch._1
      }
      val res = partChs.collect {
        case Right((_, r)) => r
      }
      curr.copy(child = newChild) -> res
    })

  def selectPath[R](predicates: Seq[xml.Elem => Boolean], st: State[xml.Elem, R]): State[xml.Elem, Seq[R]] = {
    predicates match {
      case Seq() => st.map(el => Seq(el))
      case predicate :: predicates => select[Seq[R]](predicate, selectPath(predicates, st)).map(_.flatten)
    }
  }

  def path[R](pth: Seq[String], st: State[xml.Elem, R]): State[xml.Elem, Seq[R]] = {
    selectPath[R](pth.map(nm => (el: xml.Elem) => el.label == nm), st)
  }

  def selectChildren[R](elemName: String, st: State[xml.Elem, R]) = select(el => el.label == elemName, st)

  def first[R](pred: xml.Elem => Boolean, st: State[xml.Elem, R]) = new State[xml.Elem, Option[R]]({ curr =>
    val zsq = Seq.empty[Either[xml.Node, (xml.Elem, R)]]
    val partChs = curr.child.foldLeft(zsq -> false) {
      case ((acc, found), ch: xml.Elem) if !found && pred(ch) =>
        (acc :+ Right(st.run(ch))) -> true
      case ((acc, found), ch) =>
        (acc :+ Left(ch)) -> found
    }._1
    val newChild = partChs.map {
      case Left(ch) => ch
      case Right(ch) => ch._1
    }
    val r = partChs.collectFirst {
      case Right((_, r)) => r
    }
    curr.copy(child = newChild) -> r
  })

  def selectChild[R](elemName: String, st: State[xml.Elem, R]) = first(el => el.label == elemName, st)

  def text = read[String] { el => el.text }
  def setText(v: String) = for {
    curr <- current
    _ <- write { curr.copy(child = xml.Text(v)) }
  } yield ()

  def updateText(f: String => String) = for {
    curr <- current
    txt <- text
    _ <- write { curr.copy(child = xml.Text(f(txt))) }
  } yield ()

}