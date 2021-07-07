

case class Organizer(org: List[BitMap]){
  def remove():Unit = Organizer.removeQTree(this.org)
  def addQTree(bt:BitMap):Unit = Organizer.addQTree(bt,this.org)
  def nextQTree():Unit = Organizer.nextQTree()
  def searchQTree(bt:BitMap):Unit = Organizer.searchQTree(bt, this.org)
  def previousQTree():Unit = Organizer.previousQTree()
}

object Organizer {
  var maxSize = 10

  var currentPosition = 0;

  def removeQTree(org:List[BitMap]): Unit = {
    if (org != Nil)
      org.filter(_ != org(currentPosition))
  }

  def addQTree(bt: BitMap,org:List[BitMap]): Unit = {
    if (currentPosition == maxSize){
      throw new IllegalStateException("Can't add more Images")
    } else {
     org:+bt
    }
  }

  def nextQTree(): Unit = {
    if (currentPosition + 1 > maxSize){
      throw new IllegalStateException("No More Images")
    }else {
      currentPosition = currentPosition + 1
    }
  }

  def searchQTree(bt: BitMap, org: List[BitMap]):Unit = {
    if(org.contains(bt)){
      currentPosition = org.indexOf(bt)
    } else {
      println("Image doens't exist")
    }

  }

  def previousQTree(): Unit = {
    if (currentPosition - 1 < 0 ){
      throw new IllegalStateException("No More Images")
    } else {
      currentPosition = currentPosition -1
    }
  }
}
