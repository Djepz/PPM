case class QNode[Coords](value: Coords, one: QTree[Coords], two: QTree[Coords], three: QTree[Coords], four: QTree[Coords]) extends QTree[Coords]{
  override def toString: String =
    "QNode = " + this.value +
      "\n\t - " + this.one +
      "\n\t - " + this.two +
      "\n\t - " + this.three +
      "\n\t - " + this.four
}
