package com.github.gdefacci.xmlstate

class State[S,O](val run:S => (S,O)) {
  
  def map[B](f1:O => B):State[S,B] = 
    new State(run.andThen { case (state, out) => state -> f1(out) })
  
  def flatMap[B](f1:O => State[S,B]):State[S,B] = 
    new State[S,B]( run.andThen { case (state, out) => f1(out).run(state) } )
  
}

object XmlState extends App {
  
  def read[T](f:xml.Elem => T) = new State[xml.Elem, T]( s => s -> f(s))
  def write(s1: => xml.Elem) = new State[xml.Elem, Unit]( s => (s1, ():Unit) )
  def current = new State[xml.Elem, xml.Elem]( s => (s, s) )
  
  def attribute(name:String) = read[Option[String]] { nd =>
    nd.attribute(name).map(_.text)
  }
  
  def children(name:String) = read[Seq[xml.Elem]] { nd =>
    nd.child.collect {
      case el:xml.Elem if el.label == name => el
    }
  }
  
  def childOpt(name:String) = children(name).map(_.headOption)
  def child(name:String) = children(name).map(_.head)
  
  def setName(nm:String) = for {
    curr <- current
    _ <- write( curr.copy(label = nm) )
  } yield ()
  
  def setAttr(nm:String, v:String) = for {
    curr <- current
    _ <- write( curr % xml.Attribute(None, nm, xml.Text(v), xml.Null) )
  } yield ()
  
  def replace(el:xml.Elem, elem:xml.NodeSeq) = for {
    curr <- current
    _ <- write {
      curr.copy( child = curr.child.flatMap {
        case ch:xml.Elem if (ch == el) => elem
        case x => xml.NodeSeq.fromSeq(x) 
      } )
    }
  } yield ()
  
  def insertAfter(el:xml.Elem, newl:xml.NodeSeq) = 
    replace(el, xml.NodeSeq.fromSeq(Seq(el)) ++  newl )
  
  def insertBefore(el:xml.Elem, newl:xml.Node) = 
    replace(el, newl ++ xml.NodeSeq.fromSeq(Seq(el)) )
  
  def updateAttr(nm:String, update:String => String) = for {
    curr <- current
    attr <- attribute(nm)
    _ <- attr match {
      case None => current
      case Some(v) => write( curr % xml.Attribute(None, nm, xml.Text(update(v)), xml.Null) )
    }
  } yield ()
  
  def appendChild(ch: xml.NodeSeq) = for {
    curr <- current
    _ <- write( curr.copy( child = curr.child ++ ch) )
  } yield () 
  
  def updateAll(pred:xml.Elem => Boolean, st:State[xml.Elem,Unit]) = for {
    curr <- current
    r <- write {
      val partChs:Seq[Either[xml.Node, xml.Elem]] = curr.child.collect { 
        case ch:xml.Elem if pred(ch) => Right(ch) 
        case ch => Left(ch) 
      }
      val newChild = partChs.map {
        case Left(ch) => ch
        case Right(ch) => st.run(ch)._1
      }
      curr.copy(child = newChild)
    }
  } yield r
  
  def updateChildren(elemName:String, st:State[xml.Elem,Unit]) = updateAll( el => el.label == elemName, st)

  def updateFirst(pred:xml.Elem => Boolean, st:State[xml.Elem,Unit]) = for {
    curr <- current
    r <- write {
      var found = false
      val partChs:Seq[Either[xml.Node, xml.Elem]] = curr.child.collect { 
        case ch:xml.Elem if !found && pred(ch) => 
          found = true
          Right(ch) 
        case ch => 
          Left(ch) 
      }
      val newChild = partChs.map {
        case Left(ch) => ch
        case Right(ch) => st.run(ch)._1
      }
      curr.copy(child = newChild)
    }
  } yield r
  
  def updateChild(elemName:String, st:State[xml.Elem,Unit]) = updateFirst( el => el.label == elemName, st)

  def text = read[String] { el => el.text }
  def setText(v:String) = for {
    curr <- current
    _ <- write { curr.copy( child = xml.Text(v) ) }
  } yield ()
  
  def updateText(f:String => String) = for {
    curr <- current
    txt <- text
    _ <- write { curr.copy( child = xml.Text(f(txt)) ) }
  } yield ()
  
  
}