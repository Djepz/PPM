import ImageUtil.{decodeRgb, encodeRgb, readBinaryImage, readColorImage}
import QTreeTest.{Coords, getCoords}

import java.awt.Color
import scala.annotation.tailrec
import scala.language.postfixOps

/*case class BitMap(string: String){
  def isLeaf(lst: List[List[Int]]):Boolean = BitMap.isLeaf(lst)
  def putEmptyList(lst: List[List[Int]]):List[List[Int]] = BitMap.putEmptyList(lst)
  def arrayToList(a:Array[Array[Int]]):List[List[Int]] = BitMap.arrayToList(a)
  def firstNodeAux(lst:List[List[Int]]):List[List[Int]] = BitMap.firstNodeAux(lst)
  def secondNodeAux(lst:List[List[Int]]):List[List[Int]] = BitMap.secondNodeAux(lst)
  def thirdNodeAux(lst:List[List[Int]]):List[List[Int]] = BitMap.thirdNodeAux(lst)
  def fourthNodeAux(lst:List[List[Int]]):List[List[Int]] = BitMap.fourthNodeAux(lst)
  def firstNode(lst: List[List[Int]]):QTree[Coords] = BitMap.firstNode(lst)
  def secondNode(lst: List[List[Int]]):QTree[Coords] = BitMap.secondNode(lst)
  def thirdNode(lst: List[List[Int]]):QTree[Coords] = BitMap.thirdNode(lst)
  def fourthNode(lst: List[List[Int]]):QTree[Coords] = BitMap.fourthNode(lst)
  def makeQTree():QTree[Coords] = BitMap.makeQTree(this.string)
}

object BitMap{

  def isLeaf(lst:List[List[Int]]):Boolean ={
    lst match {
      case Nil =>false
      case xs =>lst.forall(_ == xs.head)
    }
  }

  def putEmptyList(lst:List[List[Int]]):List[List[Int]] = {
    if(lst.size == lst.head.size){
      lst
    } else {
      @tailrec
      def iter(aux: List[List[Int]], acc: Int): List[List[Int]] = {
        if (acc >= 4) {
          aux
        } else {
          iter(aux:+List(), acc +1)
        }
      }

      iter(lst, lst.size)
    }
  }

  def arrayToList(a: Array[Array[Int]]): List[List[Int]] = {
    a map (_.toList) toList
  }

  def firstNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(0))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
              x.splitAt(x.size / 2)._1 :: aux(xs, list2)
        }
      }
      putEmptyList(aux(lst,Nil).splitAt(lst.size/2)._1)
    }
  }

  def secondNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(1))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
          x.splitAt(x.size / 2)._2 :: aux(xs, list2)

        }
      }
      putEmptyList(aux(lst, Nil).splitAt(lst.size / 2)._1)
    }
  }

  def thirdNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(2))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
            x.splitAt(x.size / 2)._1 :: aux(xs, list2)
        }
      }
      putEmptyList(aux(lst, Nil).splitAt(lst.size / 2)._2)
    }
  }

  def fourthNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(3))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
            x.splitAt(x.size / 2)._2 :: aux(xs, list2)
        }
      }
      putEmptyList(aux(lst, Nil).splitAt(lst.size / 2)._2)
    }
  }

  def firstNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {
            println(xs)
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._2._1/2,c._2._2/2))),
              aux(secondNodeAux(xs), ((c._2._2/2,c._1._2),(c._2._1,c._2._2/2))),
              aux(thirdNodeAux(xs),((c._1._1,xs.size/2),(c._2._1/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1/2,c._2._2/2), c._2)))
          }
      }
    }
      aux(firstNodeAux(lst),((0,0),(lst.size/2,lst.size/2)))
  }

  def secondNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {

            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.size/2,c._1._2),(c._2._1,c._2._2/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(secondNodeAux(lst),((lst.size/2,0),(lst.size,lst.size/2)))
  }

  def thirdNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(thirdNodeAux(lst),((0,lst.size/2),(lst.size/2,lst.size)))
  }
  def fourthNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(fourthNodeAux(lst),((lst.size/2,lst.size/2),(lst.size,lst.size)))
  }


  def makeQTree(string: String):QTree[Coords] = {
    val lst:List[List[Int]] = arrayToList(readColorImage(string))

    QNode(((0,0),(lst(1).size, lst.size)), firstNode(lst), secondNode(lst), thirdNode(lst), fourthNode(lst))
  }

  def main(args: Array[String]): Unit = {
    val array1 = Array(4)
    val array2 = Array(0,0,0,0)
    val array3 = Array(1,1,1,1)
    val array4 = Array(array3,array2,Array(2,8,2,1))
    val array = Array(Array(1,2,3,4), Array(5,6,7,8), Array(9,10,11,12), Array(13,14,15,16))
    val array6 = Array(Array(1),Array(2),Array(5),Array(6))
    val list = List(List(1,2,3), List(4,5,6), List(2,3,6),List(3,6,9), List(3,6,8),List(22,77,3), List())
    val trying = isLeaf(arrayToList(array))
    val qt = fourthNode(arrayToList(array))
    val qt1 = firstNodeAux(arrayToList(array))
    val dc =  decodeRgb(-1241).toList
    val l = List(List(1), List())
    val a = arrayToList(array)
    println("here" + qt1)
    println("here 1" + qt)
    println("Empty" + putEmptyList(l))

  }
}*/
case class BitMap(bit: List[List[Int]]){
  def getMatrix(): List[List[Int]] = this.bit
  def generateImageBitMap(str:String): Unit = BitMap.generateImageBitMap(this:BitMap,str)
  def makeQTree():QTree[Coords] = BitMap.makeQTree(this.bit)
}

object BitMap {

  def generateImageBitMap(bit:BitMap, str: String): Unit = {
    val data : Array[Array[Int]] = bit.getMatrix().map(_.toArray).toArray
    ImageUtil.writeImage(data, str, "png")
  }

  def isLeaf(lst:List[List[Int]]):Boolean ={
    lst match {
      case Nil =>false
      case xs =>lst.forall(_ == xs.head)
    }
  }

  def putEmptyList(lst:List[List[Int]]):List[List[Int]] = {
    if(lst.size == lst.head.size){
      lst
    } else {
      def iter(aux: List[List[Int]], acc: Int): List[List[Int]] = {
        if (acc >= 4) {
          aux
        } else {
          iter(aux, acc +1):+List()
        }
      }

      iter(lst, lst.size)
    }
  }

  def arrayToList(a: Array[Array[Int]]): List[List[Int]] = {
    a map (_.toList) toList
  }

  def firstNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(0))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
            x.splitAt(x.size / 2)._1 :: aux(xs, list2)
        }
      }
      putEmptyList(aux(lst,Nil).splitAt(lst.size/2)._1)
    }
  }

  def secondNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(1))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
            x.splitAt(x.size / 2)._2 :: aux(xs, list2)

        }
      }
      putEmptyList(aux(lst, Nil).splitAt(lst.size / 2)._1)
    }
  }

  def thirdNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(2))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
            x.splitAt(x.size / 2)._1 :: aux(xs, list2)
        }
      }
      putEmptyList(aux(lst, Nil).splitAt(lst.size / 2)._2)
    }
  }

  def fourthNodeAux(lst:List[List[Int]]):List[List[Int]] ={
    if(lst.size == 4 && lst.head.size == 1){
      lst.filter(_==lst(3))
    } else {
      def aux(lst1: List[List[Int]], list2: List[List[Int]]): List[List[Int]] = {
        lst1 match {
          case Nil => Nil
          case x :: xs =>
            x.splitAt(x.size / 2)._2 :: aux(xs, list2)
        }
      }
      putEmptyList(aux(lst, Nil).splitAt(lst.size / 2)._2)
    }
  }

  def firstNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.head.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.head.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.head.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(firstNodeAux(lst),((0,0),(lst.head.size/2,lst.size/2)))
  }

  def secondNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {

          /*  QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.size/2,c._2._2-xs.size/2), c._2))) */
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.head.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.head.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.head.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(secondNodeAux(lst),((lst.head.size/2,0),(lst.size,lst.size/2)))
  }

  def thirdNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {
           /* QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.size/2,c._2._2-xs.size/2), c._2)))*/
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.head.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.head.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.head.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(thirdNodeAux(lst),((0,lst.size/2),(lst.head.size/2,lst.size)))
  }
  def fourthNode(lst: List[List[Int]]):QTree[Coords] = {
    def aux(fn:List[List[Int]],c:Coords):QTree[Coords] = {
      fn match {
        case Nil => EmptyTree
        case Nil :: xs => EmptyTree
        case xs =>
          if (isLeaf(xs)) {
            QLeaf(c, new Color(decodeRgb(xs.head.head)(0), decodeRgb(xs.head.head)(1), decodeRgb(xs.head.head)(2)))
          } else {
           /* QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.size/2,c._2._2-xs.size/2), c._2)))*/
            QNode(c,
              aux(firstNodeAux(xs),(c._1,(c._1._1 + xs.head.size/2,c._1._2+xs.size/2))),
              aux(secondNodeAux(xs), ((c._1._1 + xs.head.size/2,c._1._2),(c._2._1,c._2._2 - xs.size/2))),
              aux(thirdNodeAux(xs),((c._1._1,c._1._2 + xs.size/2),(c._2._1 - xs.size/2, c._2._2))),
              aux(fourthNodeAux(xs),((c._2._1 - xs.head.size/2,c._2._2-xs.size/2), c._2)))
          }
      }
    }
    aux(fourthNodeAux(lst),((lst.head.size/2,lst.size/2),(lst.size,lst.size)))
  }


  def makeQTree(lst:List[List[Int]]):QTree[Coords] = {

    QNode(((0,0),(lst(1).size, lst.size)), firstNode(lst), secondNode(lst), thirdNode(lst), fourthNode(lst))
  }

}
