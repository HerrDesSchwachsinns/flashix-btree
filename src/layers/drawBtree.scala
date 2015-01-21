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

object drawBtree {
  var file = "btree.tex"
  var printDebug2 = true

  //print tree beginning at root
  def apply(root: znode): Boolean = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(file)));
    lower = null
    upper = null
    error = false
    printHeader(out)
    printNode(out, root, 0)
    printFooter(out)
    out.close()
    compile()
    return !error
  }
  private var lower: key = null
  private var upper: key = null
  private var error = false

  private def hasCorrectProps(node: znode): Boolean = {
    true
  }
  private def printNode(out: PrintWriter, node: znode, recDepth: Int) {
    out.print(intend(recDepth))
    if (recDepth == 0) out.print("\\")
    out.print("node{")

    printBranches(out, node, recDepth)
    out.println(intend(recDepth) + "}")
    printChilds(out, node, recDepth)
  }

  private def isOk(node: znode, i: Int): Boolean = {
    val branch = node.zbranches(i)
    var b = true
    if (!node.leaf && i == 0) {
      b &= branch.key == KEY_DUMMY
    } else {
      if (i > 0)
        b &= branch.key > node.zbranches(i - 1).key
      if (i < node.usedsize - 1)
        b &= branch.key < node.zbranches(i + 1).key
      if (lower != null) b &= branch.key > lower
      if (upper != null) b &= branch.key <= upper
    }
    if (b == false) error = true
    return b
  }
  private def printBranches(out: PrintWriter, node: znode, recDepth: Int) {
    for (i <- 0 to node.usedsize - 1) {
      if (i > 0) {
        out.print("\\nodepart{")
        out.print(int2text(i + 1))
        out.print("}")
      }
      val b =
        if (node.zbranches(i).key == KEY_DUMMY) "D"
        else node.zbranches(i).key

      if (!isOk(node, i))
        out.print("\\textcolor{red}{" + b + "}")
      else out.print(b)
    }
    out.println()
  }
  private def printChilds(out: PrintWriter, node: znode, recDepth: Int) {
    if (!node.leaf) {
      for (i <- 0 to node.usedsize - 1) {
        out.println(intend(recDepth) + "child{")
        lower = if (i > 0) node.zbranches(i - 1).key else null
        upper = if (i < node.usedsize - 1) node.zbranches(i + 1).key else null
        printNode(out, node.zbranches(i).child, recDepth + 1)
        out.println(intend(recDepth) + "}")
      }
    }
  }

  private def printHeader(out: PrintWriter) {
    out.println("""
    |%!TEX program = lualatex
    |\documentclass{standalone}
    |\u005cusepackage{xcolor}
    |\u005cusepackage{tikz}
    |\u005cusetikzlibrary{graphdrawing,graphdrawing.trees,shapes.multipart}
    |\begin{document}
    |\begin{tikzpicture}
    |\node{Bla};
    |\tikzstyle{bplus}=[rectangle split,
    |                   rectangle split horizontal,
    |                   rectangle split parts=8,
    |                   rectangle split ignore empty parts,
    |                   draw,
    |                   fill=white]
    |\tikzstyle{every node}=[bplus]
    |\begin{scope}%
    |[tree layout,level distance=10mm,text depth=.1em,text height=.8em]
    """.stripMargin);
  }
  private def printFooter(out: PrintWriter) {
    out.println("""
    |;
    |\end{scope}
    |\end{tikzpicture}
    |\end{document}
    """.stripMargin)
  }
  private def compile() {
    val p = Runtime.getRuntime().exec(Array("lualatex", file))
    if (printDebug2) {
      val out = new BufferedReader(new InputStreamReader(p.getInputStream()))
      var s: String = out.readLine();
      while (s != null) {
        println(s)
        s = out.readLine()
      }
    }
    p.waitFor()
  }

  private def intend(recDepth: Int): String = {
    val builder = new StringBuilder
    for (i <- 0 to recDepth) {
      builder.append(" ")
      builder.append(" ")
    }
    builder.toString
  }
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