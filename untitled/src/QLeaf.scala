case class QLeaf[Coords, Color](value: Color) extends QTree[Coords]{
  override def toString: String = "QLeaf = " + this.value
}
