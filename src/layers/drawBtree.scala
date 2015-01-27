package layers

import datatypes.znode
import datatypes.zbranch
import datatypes.key
import misc.orderedKey
import misc.KEY_DUMMY
import misc.BRANCH_SIZE
import java.io.PrintWriter
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.Arrays
import misc.Exec

object drawBtree {
  var file = "btree.tex"
  var printOutput = true

  /**
   * print tree beginning at root
   * return if one or more nodes do not satisfy the invariants of a Btree
   * checked invariants are:
   *
   */
  def apply(root: znode): Boolean = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(file)));
    error = false
    printHeader(out)
    printNode(out, root, 0, null, null)
    printFooter(out)
    out.close()
    compile()
    return !error
  }
  private var error = false

  /**
   * print a node
   * this includes printing every branch key and recursive calls to its children
   */
  private def printNode(out: PrintWriter, node: znode, recDepth: Int, lower: key, upper: key) {
    out.print(intend(recDepth))
    if(node==null){out.println("node{null}");return}
    if (node.parent == null) out.print("\\") //root node begins with backslash
    out.println("node{")
    printBranches(out, node, recDepth, lower, upper)
    out.println(intend(recDepth) + "}")
    printChildren(out, node, recDepth)
  }

  /**
   * check branch for errors and return corresponding color code
   */
  private def checkBranch(node: znode, i: Int, lower: key, upper: key): String = {
    if (node.zbranches(i) == null) { error = true; return "null-pointer" }
    val key = node.zbranches(i).key
    if (lower != null && key <= lower) { error = true; return "less-than-left" }
    if (upper != null && key > upper) { error = true; return "greater-than-right" }
    return "good"
  }

  private def printBranches(out: PrintWriter, node: znode, recDepth: Int, lower: key, upper: key) {
    for (i <- 0 to node.usedsize - 1) {
      out.print(intend(recDepth + 1))
      if (i > 0) {
        out.print(s"\\nodepart{${int2text(i + 1)}}")
      }
      //special value for dummy case
      val branch = if (!node.leaf && node.zbranches(i).key == KEY_DUMMY) "D"
      else node.zbranches(i).key

      val color = checkBranch(node, i, lower, upper)
      out.println(s"\\textcolor{$color}{$branch}")
    }
  }
  private def printChildren(out: PrintWriter, node: znode, recDepth: Int) {
    if (!node.leaf) {
      for (i <- 0 to node.usedsize - 1) {
        out.println(intend(recDepth) + "child{")
        printNode(out,
          node.zbranches(i).child,
          recDepth + 1,
          if (i > 0) node.zbranches(i).key else null,
          if (i < node.usedsize - 1) node.zbranches(i + 1).key else null)
        out.println(intend(recDepth) + "}")
      }
    }
  }

  //fuck java unicode escape policiy...
  private def printHeader(out: PrintWriter) {
    out.println("""%!TEX program = lualatex
    |\documentclass{standalone}
    |\u005cusepackage{xcolor}
    |\u005cusepackage{tikz}
    |\u005cusetikzlibrary{graphdrawing,graphdrawing.trees,shapes.multipart}
    |\begin{document}
    |\begin{tikzpicture}
    |\definecolor{good}{rgb}{0,0,0}
    |\definecolor{greater-than-right}{HTML}{CC0000}
    |\definecolor{less-than-left}{HTML}{FF6600}
    |\definecolor{null-pointer}{HTML}{0000CC}
    |\tikzstyle{bplus}=[rectangle split,
    |                   rectangle split horizontal,
    |                   rectangle split parts=12,
    |                   rectangle split ignore empty parts,
    |                   draw,
    |                   fill=white]
    |\tikzstyle{every node}=[bplus]
    |\begin{scope}%
    |[tree layout,level distance=10mm,text depth=.1em,text height=.8em]
    """.stripMargin);
  }
  private def printFooter(out: PrintWriter) {
    out.println(""";
    |\end{scope}
    |\end{tikzpicture}
    |\end{document}
    """.stripMargin)
  }
  /**
   * compile just created tex file into pdf with lualatex
   */
  private def compile() {
    Exec(Array("lualatex", file))
  }

  /**
   * intendation level
   */
  private def intend(recDepth: Int): String = {
    val builder = new StringBuilder
    for (i <- 0 to recDepth) {
      builder.append("  ")
    }
    builder.toString
  }
  /**
   * names for node parts
   */
  private def int2text(i: Int): String = i match {
    case 2  => "two"
    case 3  => "three"
    case 4  => "four"
    case 5  => "five"
    case 6  => "six"
    case 7  => "seven"
    case 8  => "eight"
    case 9  => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case _  => "???"
  }
}