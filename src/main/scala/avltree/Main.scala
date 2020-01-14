package avltree

/**
 * Created by i.zhavoronkov on 09/01/2020.
 */
object Main extends App {

  import avltree.Node.fromInt

  val tree = 100 add 15 add 190 add 3 add 91 add 13 add 17 add 171 add 205 add 155 add 303
  println(tree.toDiagram)
}
