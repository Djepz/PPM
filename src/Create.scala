import java.awt.Color
import scala.annotation.tailrec

object Create {
  type Point = (Int, Int)
  type Coords = (Point, Point)
  type Section = (Coords, Color)

  def qTreeSize(qt: QTree[Coords]): (Int, Int) = {
    qt match {
      case EmptyTree => (0, 0)
      case QLeaf((((_, _), (x2: Int, y2: Int)), _)) => (x2  , y2  )
      case QNode(((_, _), (x2: Int, y2: Int)), _, _, _, _) => (x2  , y2  )
    }
  }

  def makeBitMap(qt: QTree[Coords]): BitMap = {
    val list: Array[Array[Int]] = Array.ofDim[Int](qTreeSize(qt)._1,qTreeSize(qt)._2)

    def makeArray(qt2: QTree[Coords]): Unit = {
      qt2 match {
        case EmptyTree => Nil
        case QLeaf((((x1: Int, y1: Int), (x2: Int, y2: Int)), color: Color)) => {

          def aux1(coords: Coords): Unit = {
            @tailrec
            def aux2(coords: Coords): Unit = {
              if (coords._1._2 <= y2 - 1) {
                list(coords._1._2)(coords._1._1) = ImageUtil.encodeRgb(color.getRed, color.getGreen, color.getBlue)
                aux2((coords._1._1, coords._1._2 + 1), (x2, y2))
              } else {
                aux1((coords._1._1 + 1 , y1), (x2, y2))
              }
            }

            if (coords._1._1 <= x2 -1)
              aux2((coords._1._1, coords._1._2), (x2, y2))
          }

          aux1(((x1, y1), (x2, y2)))
        }
        case QNode(_, one, two, three, four) =>
          makeArray(one)
          makeArray(two)
          makeArray(three)
          makeArray(four)
      }
    }

    makeArray(qt)
    new BitMap(list.map(_.toList).toList)
  }
}
