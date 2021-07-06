
import ImageUtil.encodeRgb
import QTreeTest.{Coords, cropTool}

import java.awt.Color
import java.util.Random
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.math.{exp, sqrt}
import scala.util.Random.javaRandomToRandom



case class QTreeTest(qt: QTree[Coords]){
  def scaleQTree(scale:Double):QTree[Coords] = QTreeTest.scaleQTree(scale,this.qt)
  /*def rotateTreeRight():QTree[Coords] = QTreeTest.rotateTreeRight(this.qt)
  def rotateTreeRight():QTree[Coords] = QTreeTest.rotateTreeRight(this.qt)
  def rotateTreeLeft():QTree[Coords] = QTreeTest.rotateTreeLeft(this.qt)
  def mirrorVTree():QTree[Coords] = QTreeTest.mirrorVTree(this.qt)
  def mirrorHTree():QTree[Coords] = QTreeTest.mirrorHTree(this.qt)*/
  def mapColourEffect(f:Color => Color):QTree[Coords] = QTreeTest.mapColourEffect(f,this.qt)
  def getCoords():Coords = QTreeTest.getCoords(this.qt)
  def changeCoords(c:Coords): QTree[Coords] = QTreeTest.changeCoords(this.qt,c)
  def foldTree(f: (QTree[Coords], QTree[Coords], Coords, QTree[Coords], QTree[Coords]) => QTree[Coords]): QTree[Coords] = QTreeTest.foldTree(this.qt)(f)
  //def sepia(c:Color):Color = QTreeTest.sepia(c)
  //def contrast(c:Color, x:Int):Color = QTreeTest.contrast(c,x)
  def noise(c:Color):Color = QTreeTest.noise(c)
  def vignette(c:Color):Color = QTreeTest.vignette(this.qt,c)
}

object QTreeTest {

  type Point = (Int, Int)
  type Coords = (Point, Point)
  type Section = (Coords, Color)

  def limit(a: Int, b: Int): Int = {
    if (a - b > 0 ) {
      a - b
    } else {
      0
    }
  }

  def limit1(a: Int, b: Int): Int = {
    if (b > a) {
      a
    } else {
      b
    }
  }

  def limit2(a: Int, b: Int): Int = {
    if (b < a) {
      a
    } else {
      b
    }
  }





  def getWidth(qt:QTree[Coords]): Int = {
    qt match {
      case EmptyTree => 0
      case QLeaf(section: Section) => section._1._2._1
      case QNode(value,_,_,_,_) => value._2._1
    }
  }



  def getHeigth(qt:QTree[Coords]):Int = {
    qt match {
      case EmptyTree => 0
      case QLeaf(section: Section) => section._1._2._2
      case QNode(value,_,_,_,_) => value._2._2
    }
  }



  def getColor(qt:QTree[Coords]):Option[Color] ={
    qt match {
      case EmptyTree => None
      case QLeaf(section:Section) => Some(section._2)
    }
  }



  def getCoords(qt: QTree[Coords]): Coords = {
    qt match {
      case EmptyTree => ((0, 0), (0, 0))
      case QLeaf(section: Section) => section._1
      case QNode(value, _, _, _, _) => value
    }
  }



  def changeCoords(qt: QTree[Coords], coords: Coords): QTree[Coords] = {
    qt match {
      case EmptyTree => EmptyTree
      case QLeaf(section: Section) => QLeaf(coords,section._2)
      case QNode(_, one, two, three, four) => QNode(coords, one, two, three, four)
    }
  }




    def scaleQTree(scale:Double, qt:QTree[Coords]):QTree[Coords]={
      qt match {
        case EmptyTree => EmptyTree
        case QLeaf(section: Section) =>
          val newP1 = (section._1._1._1 * scale).round.toInt
          val newP2 = (section._1._1._2 * scale).round.toInt
          val newP3 = (section._1._2._1 * scale).round.toInt
          val newP4 = (section._1._2._2 * scale).round.toInt
          val newCoords: Coords = ((newP1, newP2), (newP3, newP4))
          changeCoords(QLeaf(section), newCoords)
        case QNode(value, one, two, three, four) =>
          val newC1 = (value._1._1 * scale).round.toInt
          val newC2 = (value._1._2 * scale).round.toInt
          val newC3 = (value._2._1 * scale).round.toInt
          val newC4 = (value._2._2 * scale).round.toInt
          val newCoords1: Coords = ((newC1, newC2), (newC3, newC4))
          changeCoords(QNode(value, scaleQTree(scale,one), scaleQTree(scale,two), scaleQTree(scale,three), scaleQTree(scale,four)), newCoords1)
      }
    }



    def foldTree[Coords](t: QTree[Coords])(f: (QTree[Coords], QTree[Coords], Coords, QTree[Coords], QTree[Coords]) => QTree[Coords]): QTree[Coords] = {
      t match {
        case EmptyTree => EmptyTree
        case QLeaf(section: Section) => QLeaf(section)
        case QNode(x, one, two, three, four) => f(foldTree(one)(f), foldTree(two)(f), x, foldTree(three)(f), foldTree(four)(f))
      }
    }

    def cropHelper(qt: QTree[Coords], c:Coords): QTree[Coords] = {
      qt match {
        case EmptyTree => EmptyTree
        case QLeaf(section: Section) =>
          if(section._1._1._1 >= c._1._1 && section._1._1._2 >= c._1._2  && section._1._2._1 <= c._2._1 && section._1._2._2 <= c._2._2) {
            QLeaf(section)
          } else if(section._1._1._1 >= c._1._1 && section._1._1._2 >= c._1._2) {
            QLeaf(((section._1._1._1, section._1._1._2), c._2), section._2)
          //} else if(section._1._2._1 <= c._2._1 && section._1._2._2 <= c._2._2 && section._1._1._1 >= c._1._1 && section._1._1._2 <= c._1._2){
            //QLeaf((c._1,(section._1._2._1,section._1._2._2)),section._2)
          } else
            EmptyTree
        case QNode(value, one, two, three, four) =>
            QNode(value
              , cropHelper (one, c), cropHelper (two, c), cropHelper (three, c), cropHelper (four, c))
      }
    }

    def cropAdjust(qt:QTree[Coords], c:Coords): QTree[Coords] = {
      qt match {
        case EmptyTree => EmptyTree
        case QLeaf(section: Section) => QLeaf(((section._1._1._1 - c._1._1,section._1._1._2 - c._1._2),(section._1._2._1 - c._1._1,section._1._2._2 - c._1._2)),section._2)
        case QNode(value, one, two, three, four) =>

          if (one == EmptyTree && two == EmptyTree && three == EmptyTree && four == EmptyTree) {
            EmptyTree
          } else if (one != EmptyTree && two == EmptyTree && three == EmptyTree && four == EmptyTree) {
            changeCoords(one,((limit(getCoords(one)._1._1, c._1._1), limit(getCoords(one)._1._2, c._1._2)),(limit(getCoords(one)._2._1, c._1._1),limit(getCoords(one)._2._2 , c._1._2))))
          } else if (one == EmptyTree && two != EmptyTree && three == EmptyTree && four == EmptyTree) {
            changeCoords(two,((limit(getCoords(two)._1._1, c._1._1), limit(getCoords(two)._1._2, c._1._2)),(limit(getCoords(two)._2._1, c._1._1),limit(getCoords(two)._2._2 , c._1._2))))
          } else if (one == EmptyTree && two == EmptyTree && three != EmptyTree && four == EmptyTree) {
            changeCoords(three,((limit(getCoords(three)._1._1, c._1._1), limit(getCoords(three)._1._2, c._1._2)),(limit(getCoords(three)._2._1, c._1._1),limit(getCoords(three)._2._2 , c._1._2))))
          } else if (one == EmptyTree && two == EmptyTree && three == EmptyTree && four != EmptyTree){
            changeCoords(four,((limit(getCoords(four)._1._1, c._1._1), limit(getCoords(four)._1._2, c._1._2)),(limit(getCoords(four)._2._1, c._1._1),limit(getCoords(four)._2._2 , c._1._2))))
          } else {
          QNode(((limit(value._1._1, c._1._1), limit(value._1._2, c._1._2)),(limit(value._2._1, c._1._1),limit(value._2._2 , c._1._2))),
          cropAdjust(cropHelper(one, c),c), cropAdjust(cropHelper(two, c),c),cropAdjust(cropHelper(three, c),c),cropAdjust(cropHelper(four, c),c))
        }
      }
    }

    def cropTool(qt:QTree[Coords],c:Coords):QTree[Coords] = {
      val crop: QTree[Coords] =cropAdjust(qt,c)
      changeCoords(crop,((0,0),(c._2._1 - c._1._1,c._2._2 - c._1._2)))
    }

   def vignette(qt:QTree[Coords], c:Color):Color = {
     val cx = 0.5*getCoords(qt)._2._1
     val cy = 0.5*getCoords(qt)._2._2
     val maxDist = 1.0/sqrt(cx*cx+cy*cy);



      qt match {
        case EmptyTree => vignette(qt,c)
        case QLeaf(section: Section) =>
          val dist = sqrt((section._1._2._1-cx)*(section._1._2._1-cx)+(section._1._2._2-cy)*(section._1._2._2-cy));
          val lumen = 0.75 / (1.0 + exp((dist * maxDist - 0.73) * 20.0)) + 0.25

          val newRed =limit1(255,(c.getRed * lumen).toInt)
          val newGreen =limit1(255,(c.getGreen * lumen).toInt)
          val newBlue =limit1(255,(c.getBlue * lumen).toInt)

          new Color(newRed,newGreen,newBlue)
        case QNode(_,one,two,three,four) =>
          vignette(one,c)
          vignette(two,c)
          vignette(three,c)
          vignette(four,c)
    }
  }


  /*  def rotateTreeRight(qt: QTree[Coords]): QTree[Coords] = {
      foldTree(qt) { (one, two, value, three, four) =>
        val c: Coords = getCoords(one)
        val b: Coords = getCoords(two)
        val a: Coords = getCoords(three)
        val d: Coords = getCoords(four)
        QNode(value, changeCoords(three,c), changeCoords(one,b), changeCoords(four,a), changeCoords(two,d))}
    }

    def rotateTreeLeft(qt: QTree[Coords]): QTree[Coords] = {
      foldTree(qt) { (one, two, value, three, four) =>
        val c: Coords = getCoords(one)
        val b: Coords = getCoords(two)
        val a: Coords = getCoords(three)
        val d: Coords = getCoords(four)
        QNode(value, changeCoords(two, c) , changeCoords(four,b), changeCoords(one,a), changeCoords(three,d))}
    }

    def mirrorVTree(qt: QTree[Coords]): QTree[Coords] = {
      foldTree(qt) { (one, two, value, three, four) =>
        val c: Coords = getCoords(one)
        val b: Coords = getCoords(two)
        val a: Coords = getCoords(three)
        val d: Coords = getCoords(four)
        QNode(value, changeCoords(two,c), changeCoords(one,b), changeCoords(four,a), changeCoords(three,d)) }
    }

    def mirrorHTree(qt: QTree[Coords]): QTree[Coords] = {
      foldTree(qt) { (one, two, value, three, four) =>
        val c: Coords = getCoords(one)
        val b: Coords = getCoords(two)
        val a: Coords = getCoords(three)
        val d: Coords = getCoords(four)
        QNode(value, changeCoords(three,c), changeCoords(four,b), changeCoords(one,a), changeCoords(two,d)) }
    }

    def sepia(c: Color): Color = {
      val newRed = limit(255, (0.393 * c.getRed + 0.769 * c.getGreen + 0.189 * c.getBlue).toInt)
      val newGreen = limit(255,(0.349 * c.getRed + 0.686 * c.getGreen + 0.168 * c.getBlue).toInt)
      val newBlue = limit(255,(0.272 * c.getRed + 0.534 * c.getGreen + 0.131 * c.getBlue).toInt)
      new Color(newRed, newGreen, newBlue)
    }

    def contrast(c: Color, x: Int): Color = {
      val factor = (259 * (x + 255)) / (255 * (259 - x))
      val newRed = limit(255, factor * (c.getRed - 128) + 128)
      val newGreen = limit(255,factor * (c.getGreen - 128) + 128)
      val newBlue = limit(255,factor * (c.getBlue - 128) + 128)

      new Color(newRed, newGreen, newBlue)
    }*/

    def noise(c:Color):Color ={
      val r = Math.random()*100

      if(r > 50)  {
        val newRed = limit1(255, (c.getRed + r ).toInt)
        val newGreen = limit1(255,(c.getGreen + r).toInt)
        val newBlue = limit1(255, (c.getBlue + r).toInt)
        new Color(newRed, newGreen, newBlue)
      } else {
        val newRed = limit2(0,(c.getRed - r).toInt)
        val newGreen = limit2(0,(c.getGreen - r).toInt)
        val newBlue = limit2(0,(c.getBlue - r).toInt)
        new Color(newRed, newGreen, newBlue)
      }
    }

  def contrast(c: Color, x: Int): Color = {
    val factor = (259 * (x + 255)) / (255 * (259 - x))
    val newRed = limit(255, factor * (c.getRed - 128) + 128)
    val newGreen = limit(255,factor * (c.getGreen - 128) + 128)
    val newBlue = limit(255,factor * (c.getBlue - 128) + 128)

    new Color(newRed, newGreen, newBlue)
  }

    def mapColourEffect(f:Color => Color, qt:QTree[Coords]):QTree[Coords] = {
      qt match {
        case EmptyTree => EmptyTree
        case QLeaf(section: Section) => QLeaf(section._1, f(section._2))
        case QNode(value, one, two, three, four) =>
          QNode(value, mapColourEffect(f, one), mapColourEffect(f, two), mapColourEffect(f, three), mapColourEffect(f, four))
      }
  }
}



