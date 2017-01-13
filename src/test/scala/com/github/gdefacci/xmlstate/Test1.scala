package com.github.gdefacci.xmlstate

import org.scalatest.FunSuite

import XmlState._
import java.io.InputStreamReader

class Test1 extends FunSuite {

  test("sample") {
    
    val tx1 = for {
      _ <- selectChildren("pippo", for {
        _ <- setAttr("a", "b")
        _ <- appendChild(<roo>yehh</roo>)
        age <- child("age")
        _ <- insertBefore(age, <street>pipppo</street>)
        _ <- insertAfter(age, <city>new york</city>)
        _ <- selectChild("language", for {
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

  def load(pth:String) = xml.XML.load(Thread.currentThread().getContextClassLoader.getResourceAsStream(pth))
  
  test("planets") {
    
    val tx = for {
      satellites <- path("planets" :: "planet" :: "satellites" :: "satellite" :: "name" :: Nil, text)
    } yield satellites 
    
    assert( tx.run(load("planets.xml"))._2.toSet == Set("moon", "phobos", "deimos", "iii ganymede", "iv callisto", "titan", "rhea", "ariel", "miranda", "triton") )
  
  }
  
  test("planets1") {
    val tx1 = for {
      _ <- path("planets" :: "planet" :: "satellites" :: "satellite" :: "name" :: Nil, updateText { x => x.charAt(0).toUpper + x.substring(1) })
      satellites <- path("planets" :: "planet" :: "satellites" :: "satellite" :: "name" :: Nil, text)
    } yield satellites 
  
    assert( tx1.run(load("planets.xml"))._2.toSet == Set("Moon", "Phobos", "Deimos", "Iii ganymede", "Iv callisto", "Titan", "Rhea", "Ariel", "Miranda", "Triton") )
  }
  
  test("planets2") {
    val tx = for {
      yellowPlanets <- selectPath(ByName("planets") :: (ByName("planet") and ByAttribute("color", _ == "yellow")) :: Nil, for {
        nm <- selectChild("name", text)        
        lengthOfYear <- selectChild("lengthOfYear", text)        
      } yield  { 
        <planet name={nm.map(xml.Text(_))} year={lengthOfYear.map(xml.Text(_))} /> 
      })
      _ <- write( <yellowPlanets>{yellowPlanets}</yellowPlanets> )
    } yield <yellow>{yellowPlanets}</yellow>
    
    println( tx.run(load("planets.xml"))._2 )
    println( tx.run(load("planets.xml"))._1 )
    
  }
  
  
}