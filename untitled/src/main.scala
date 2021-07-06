import QTreeTest.{Coords, Point, Section, noise, vignette}

import java.awt.Color


object main {

  val directory: String = System.getProperty("user.dir")
  var imgpath: String = directory + "\\src\\Imagens\\"


  val l1: QLeaf[Coords, Section] = QLeaf((((0, 0): Point, (1, 1): Point): Coords, Color.red): Section)
  val l2: QLeaf[Coords, Section] = QLeaf((((1, 0): Point, (2, 1): Point): Coords, Color.blue): Section)
  val l3: QLeaf[Coords, Section] = QLeaf((((0, 1): Point, (1, 2): Point): Coords, Color.yellow): Section)
  val l4: QLeaf[Coords, Section] = QLeaf((((1, 1): Point, (2, 2): Point): Coords, Color.green): Section)

  val l5: QLeaf[Coords, Section] = QLeaf((((2, 0): Point, (3, 1): Point): Coords, Color.red): Section)
  val l6: QLeaf[Coords, Section] = QLeaf((((3, 0): Point, (4, 1): Point): Coords, Color.blue): Section)
  val l7: QLeaf[Coords, Section] = QLeaf((((2, 1): Point, (3, 2): Point): Coords, Color.yellow): Section)
  val l8: QLeaf[Coords, Section] = QLeaf((((3, 1): Point, (4, 2): Point): Coords, Color.green): Section)

  val l9: QLeaf[Coords, Section] = QLeaf((((0, 2): Point, (1, 3): Point): Coords, Color.red): Section)
  val l10: QLeaf[Coords, Section] = QLeaf((((1, 2): Point, (2, 3): Point): Coords, Color.blue): Section)
  val l11: QLeaf[Coords, Section] = QLeaf((((0, 3): Point, (1, 4): Point): Coords, Color.yellow): Section)
  val l12: QLeaf[Coords, Section] = QLeaf((((1, 3): Point, (2, 4): Point): Coords, Color.green): Section)

  val l13: QLeaf[Coords, Section] = QLeaf((((2, 2): Point, (3, 3): Point): Coords, Color.red): Section)
  val l14: QLeaf[Coords, Section] = QLeaf((((3, 2): Point, (4, 3): Point): Coords, Color.blue): Section)
  val l15: QLeaf[Coords, Section] = QLeaf((((2, 3): Point, (3, 4): Point): Coords, Color.yellow): Section)
  val l16: QLeaf[Coords, Section] = QLeaf((((3, 3): Point, (4, 4): Point): Coords, Color.green): Section)


  val lq: QLeaf[Coords, Section] = QLeaf((((0,0):Point,(0,0):Point):Coords, Color.red):Section)
  val lw: QLeaf[Coords, Section] = QLeaf((((1,0):Point,(1,0):Point):Coords, Color.blue):Section)
  val le: QLeaf[Coords, Section] = QLeaf((((0,1):Point,(0,1):Point):Coords, Color.yellow):Section)
  val lr: QLeaf[Coords, Section] = QLeaf((((1,1):Point,(1,1):Point):Coords, Color.green):Section)

  val qt1: QTree[Coords] = QNode(((0, 0), (4, 4)), QNode(((0,0),(2,2)),l1,l2,l3,l4), QNode(((2,0),(4,2)),l5,l6,l7,l8), QNode(((0,2),(2,4)),l9,l10,l11,l12),
    QNode(((2,2),(4,4)),l13,l14,l15,l16))




  val bit: BitMap = BitMap({
   val image: Array[Array[Int]] = ImageUtil.readColorImage(imgpath + "test.png")
   image.map(_.toList).toList
  })

  val bit6: BitMap = BitMap({
    val image: Array[Array[Int]] = ImageUtil.readColorImage(imgpath + "test2.png")
    image.map(_.toList).toList
  })

  val qt: QTree[Coords] = BitMap.makeQTree(bit.bit)
  //val bitscale: qt2.scale()
  //val bit1: BitMap = Create.makeBitMap(qt2)
  //val bit2: BitMap = Create.makeBitMap(qt1)

  //val test1: BitMap = Create.makeBitMap(qt2)



  val qtcrop:QTree[Coords] = QTreeTest.cropTool(qt, ((15,15),(30,30)))
  val bit1: BitMap = Create.makeBitMap(qtcrop)

  val qtnoise: QTree[Coords] = QTreeTest.mapColourEffect(noise,qt1)
  val bit3: BitMap = Create.makeBitMap(qtnoise)


 // val qt: QTree[Coords] = BitMap.makeQTree(bit.bit)
  val qt2:QTree[Coords] = BitMap.makeQTree(bit6.bit)
 // val bit7: BitMap = Create.makeBitMap(qt2)

  val list = List(List(1,2,3,4), List(4,5,6,8), List(9,10,11,12),List(13,14,15,16))

  def main(args: Array[String]): Unit = {


    bit1.generateImageBitMap(imgpath + "imageCrop.png")

    bit.generateImageBitMap(imgpath + "imagesvignette.png")
    bit6.generateImageBitMap(imgpath + "imagesTest2og.png")



  // Edição Teste
  }
}

