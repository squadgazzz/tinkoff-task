package avltree

import scala.annotation.tailrec

/**
 * Created by i.zhavoronkov on 09/01/2020.
 */
sealed trait AVLTree[+A] {
  def balance: Int

  def depth: Int

  def add[B >: A](value: B)(implicit ordering: Ordering[B]): AVLTree[B]

  def rebalance[B >: A]: AVLTree[B]

  def leftRotation[B >: A]: Node[B]

  def rightRotation[B >: A]: Node[B]

  def search[B >: A](value: B): Option[Node[B]]

  def toDiagram: String
}

case object Leaf extends AVLTree[Nothing] {
  override def balance: Int = 0

  override def depth: Int = -1

  override def add[B >: Nothing](value: B)(implicit ordering: Ordering[B]): AVLTree[B] = Node(value, Leaf, Leaf)

  override def leftRotation[B >: Nothing]: Node[B] = sys.error("leaf left rotation")

  override def rightRotation[B >: Nothing]: Node[B] = sys.error("leaf right rotation")

  override def rebalance[B >: Nothing]: AVLTree[B] = this

  override def search[B >: Nothing](value: B): Option[Node[B]] = None

  override def toDiagram: String = ""
}

case class Node[+A](data: A, left: AVLTree[A], right: AVLTree[A]) extends AVLTree[A] {

  def this(data: A) = this(data, Leaf, Leaf)

  override def balance: Int = left.depth - right.depth

  override def depth: Int = math.max(left.depth, right.depth) + 1

  override def add[B >: A](value: B)(implicit ordering: Ordering[B]): AVLTree[B] = {
    val ord = ordering.compare(value, data)
    val newTree =
      if (ord == 0) {
        this
      } else if (ord > 0) {
        Node(data, left, right.add(value)(ordering))
      } else {
        Node(data, left.add(value)(ordering), right)
      }
    newTree.rebalance
  }

  override def rebalance[B >: A]: AVLTree[B] = {
    val treeBalance = balance
    if (treeBalance == 2) {
      val lBalance = left.balance
      if (lBalance > 0) {
        rightRotation
      } else if (lBalance < 0) {
        val newL = left.leftRotation
        Node(data, newL, right).rightRotation
      } else {
        this
      }
    } else if (treeBalance == -2) {
      val rBalance = right.balance
      if (rBalance < 0) {
        leftRotation
      } else if (rBalance > 0) {
        val newR = right.rightRotation
        Node(data, left, newR).leftRotation
      } else {
        this
      }
    } else {
      this
    }
  }

  override def leftRotation[B >: A]: Node[B] = this match {
    case Node(a, l, Node(b, m, r)) => Node(b, Node(a, l, m), r)
    case _ => sys.error("node left rotation error")
  }

  override def rightRotation[B >: A]: Node[B] = this match {
    case Node(a, Node(b, l, m), r) => Node(b, l, Node(a, m, r))
    case _ => sys.error("node right rotation error")
  }

  override def toDiagram: String = {
    @tailrec
    def _toDiagram(nodes: List[AVLTree[A]], acc: List[List[String]]): List[List[String]] = {
      if (nodes.isEmpty)
        acc
      else {
        val newAcc = acc :+ nodes.collect {
          case Node(d, _, _) => d.toString
          case _ => ""
        }
        val newNodes = nodes.flatMap {
          case Node(_, l, r) => List(l, r)
          case _ => List.empty
        }

        _toDiagram(newNodes, newAcc)
      }
    }

    def toDiagramElement(node: String, maxChars: Int): String = {
      if (node.isEmpty)
        " " * maxChars
      else {
        val remainSpaces = maxChars - node.length
        if (remainSpaces > 0) {
          val head = " " * (remainSpaces / 2)
          if (remainSpaces == 1)
            " " + node
          else if (remainSpaces % 2 == 0)
            head + node + head
          else
            " " + head + node + head
        } else node
      }
    }

    val levelsWithLeafs = _toDiagram(List(this), List.empty)
    val levels = if (levelsWithLeafs.last.mkString("").isEmpty) levelsWithLeafs.init else levelsWithLeafs
    val maxChars = levels.flatten.map(_.length).max
    val maxSize = levels.last.size
    levels.map(level => {
      val spacesCount = maxChars * maxSize * 2 / (level.size * 2) - maxChars
      val delimiter = if (spacesCount > 0) " " * spacesCount.toInt else ""
      delimiter + level.map(toDiagramElement(_, maxChars))
        .flatMap(c => List(c, " " * maxChars))
        .mkString(delimiter)
    }).mkString("\n")
  }

  override def search[B >: A](value: B): Option[Node[B]] = {
    @tailrec
    def _search(nodes: List[Node[B]]): Option[Node[B]] = {
      if (nodes.isEmpty)
        None
      else {
        val found = nodes.find {
          case Node(d, _, _) if value == d => true
          case _ => false
        }

        if (found.isDefined) found
        else {
          val newNodes = nodes.flatMap {
            case Node(_, l, r) => List(l, r)
          }.filter(_ != Leaf).map(_.asInstanceOf[Node[B]])
          _search(newNodes)
        }
      }
    }

    _search(List(this))
  }
}

object Node {
  def apply[A](data: A): Node[A] = new Node(data)

  implicit def fromInt(i: Int): Node[Int] = Node(i)
}