package com.github.gdefacci.xmlstate

import org.scalatest.FunSuite

import XmlState._

class Test1 extends FunSuite {

  test("sample") {
    
    val tx1 = for {
      _ <- updateChildren("pippo", for {
        _ <- setAttr("a", "b")
        _ <- appendChild(<roo>yehh</roo>)
        age <- child("age")
        _ <- insertBefore(age, <street>pipppo</street>)
        _ <- insertAfter(age, <city>new york</city>)
        _ <- updateChild("language", for {
          _ <- setText("English")
          _ <- updateAttr("level", "very " + _)
        } yield ())
      } yield ())
    } yield 2

    val xml1 = <Abba><pippo><name>blah</name><age>544</age><language level="good">french</language></pippo></Abba>

    val prntr = new xml.PrettyPrinter(0, 0)

    val expected = """<Abba><pippo a="b"><name>blah</name><street>pipppo</street><age>544</age><city>new york</city><language level="very good">English</language><roo>yehh</roo></pippo></Abba>"""
    
    println(new xml.PrettyPrinter(100, 2).format(xml1))
    println(new xml.PrettyPrinter(100, 2).format(tx1.run(xml1)._1))
    assert(
      expected.trim
      == 
      prntr.format(tx1.run(xml1)._1).trim)

  }

}