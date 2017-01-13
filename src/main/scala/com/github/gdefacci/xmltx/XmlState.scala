package com.github.gdefacci.xmlstate

import scala.xml.Elem

class State[S, O](val run: S => (S, O)) {

  def map[B](f1: O => B): State[S, B] =
    new State(run.andThen { case (state, out) => state -> f1(out) })

  def flatMap[B](f1: O => State[S, B]): State[S, B] =
    new State[S, B](run.andThen { case (state, out) => f1(out).run(state) })

}

object XmlPredicate {
  def apply(p: Elem => Boolean) = new XmlPredicate {
    def apply(el: Elem): Boolean = p(el)
  }
}

trait XmlPredicate extends (Elem => Boolean) { self =>

  def and(p: Elem => Boolean) = XmlPredicate(el => self.apply(el) && p.apply(el))

  def or(p: Elem => Boolean) = XmlPredicate(el => self.apply(el) || p.apply(el))

  def not = XmlPredicate(el => !self.apply(el))
}

object XmlState extends App {

  case class ByName(name: String) extends XmlPredicate {
    def apply(el: Elem) = el.label == name
  }

  case class ByAttribute(name: String, predicate: String => Boolean) extends XmlPredicate {
    def apply(el: Elem) = el.attribute(name).map(el => predicate(el.text)).getOrElse(false)
  }

  def get = new State[Elem, Elem](s => s -> s)
  def set(s1: => Elem) = new State[Elem, Unit](s => (s1, (): Unit))
  def modify(f: Elem => Elem) = new State[Elem, Unit](s => (f(s), (): Unit))

  def attribute(name: String) = get.map { nd =>
    nd.attribute(name).map(_.text)
  }

  def childElements(predicate: Elem => Boolean) = get.map { nd =>
    nd.child.collect {
      case el: Elem if predicate(el) => el
    }
  }
  def childElementOpt(predicate: Elem => Boolean): State[Elem, Option[Elem]] = childElements(predicate).map(_.headOption)
  def childElement(predicate: Elem => Boolean): State[Elem, Elem] = childElements(predicate).map(_.head)

  def children(name: String) = childElements(_.label == name)
  def childOpt(name: String) = children(name).map(_.headOption)
  def child(name: String) = children(name).map(_.head)

  def setName(nm: String) = modify( curr => curr.copy(label = nm))

  def setAttr(nm: String, v: String) = modify( curr => curr % xml.Attribute(None, nm, xml.Text(v), xml.Null))

  def replace(el: Elem, elem: xml.NodeSeq) = modify  { curr =>
    curr.copy(child = curr.child.flatMap {
      case ch: Elem if (ch == el) => elem
      case x => xml.NodeSeq.fromSeq(x)
    })
  }

  def insertAfter(el: Elem, newl: xml.NodeSeq) =
    replace(el, xml.NodeSeq.fromSeq(Seq(el)) ++ newl)

  def insertBefore(el: Elem, newl: xml.Node) =
    replace(el, newl ++ xml.NodeSeq.fromSeq(Seq(el)))

  def updateAttr(nm: String, update: String => String) = for {
    attr <- attribute(nm)
    _ <- attr match {
      case None => get
      case Some(v) => modify(curr => curr % xml.Attribute(None, nm, xml.Text(update(v)), xml.Null))
    }
  } yield ()

  def appendChild(ch: xml.NodeSeq) = modify { curr =>
    curr.copy(child = curr.child ++ ch)
  } 
  
  def select[R](pred: Elem => Boolean, st: State[Elem, R]) =
    new State[Elem, Seq[R]]({ curr =>
      val partChs: Seq[Either[xml.Node, (Elem, R)]] = curr.child.collect {
        case ch: Elem if pred(ch) => Right(st.run(ch))
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

  def selectPath[R](predicates: Seq[Elem => Boolean], st: State[Elem, R]): State[Elem, Seq[R]] = {
    predicates match {
      case Seq() => st.map(el => Seq(el))
      case predicate :: predicates => select[Seq[R]](predicate, selectPath(predicates, st)).map(_.flatten)
    }
  }

  def path[R](pth: Seq[String], st: State[Elem, R]): State[Elem, Seq[R]] = {
    selectPath[R](pth.map(nm => (el: Elem) => el.label == nm), st)
  }

  def selectChildren[R](elemName: String, st: State[Elem, R]) = select(el => el.label == elemName, st)

  def first[R](pred: Elem => Boolean, st: State[Elem, R]) = new State[Elem, Option[R]]({ curr =>
    val zsq = Seq.empty[Either[xml.Node, (Elem, R)]]
    val partChs = curr.child.foldLeft(zsq -> false) {
      case ((acc, found), ch: Elem) if !found && pred(ch) =>
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

  def selectChild[R](elemName: String, st: State[Elem, R]) = first(el => el.label == elemName, st)

  def text = get.map { el => el.text }
  def setText(v: String) = modify { curr => curr.copy(child = xml.Text(v)) }

  def updateText(f: String => String) = for {
    txt <- text
    _ <- modify { curr => curr.copy(child = xml.Text(f(txt))) }
  } yield ()

}