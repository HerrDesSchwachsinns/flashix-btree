package layers

import datatypes.znode
import misc.KEY_DUMMY
import java.io.PrintWriter
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.Arrays

object drawBtree {
  var file = "btree.tex"
  var printDebug2 = true
  def apply(root: znode) {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(file)));
    printHeader(out)
    printNode(out, root, 0)
    printFooter(out)
    out.close()
    compile()

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
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case _ => "???"
  }
  private def printNode(out: PrintWriter, node: znode, recDepth: Int) {
    out.print(intend(recDepth))
    if (recDepth == 0) out.print("\\")
    out.print("node{")
    printBranches(out, node, recDepth)
    out.println(intend(recDepth) + "}")
    printChilds(out, node, recDepth)
  }
  private def printBranches(out: PrintWriter, node: znode, recDepth: Int) {
    for (i <- 0 to node.usedsize - 1) {
      if (i > 0) {
        out.print("\\nodepart{")
        out.print(int2text(i + 1))
        out.print("}")
      }
      if (node.zbranches(i).key == KEY_DUMMY)
        out.print("D")
      else
        out.print(node.zbranches(i).key)
    }
    out.println()
  }
  private def printChilds(out: PrintWriter, node: znode, recDepth: Int) {
    if (!node.leaf) {
      for (i <- 0 to node.usedsize - 1) {
        out.println(intend(recDepth) + "child{")
        printNode(out, node.zbranches(i).child, recDepth + 1)
        out.println(intend(recDepth) + "}")
      }
    }
  }

  private def printHeader(out: PrintWriter) {
    out.println("%!TEX program = lualatex")
    out.println("\\documentclass{standalone}")
    out.println("\\usepackage{tikz}")
    out.println("\\usetikzlibrary{graphdrawing,graphdrawing.trees,shapes.multipart}")
    out.println("\\begin{document}")
    out.println("\\begin{tikzpicture}")
    out.println("\\tikzstyle{bplus}=[rectangle split, rectangle split horizontal,rectangle split parts=8,rectangle split ignore empty parts,draw, fill=white]")
    out.println("\\tikzstyle{every node}=[bplus]")
    out.println("\\begin{scope}%")
    out.println("[tree layout,level distance=10mm,text depth=.1em,text height=.8em]")

  }
  private def printFooter(out: PrintWriter) {
    out.println(";")
    out.println("\\end{scope}")
    out.println("\\end{tikzpicture}")
    out.println("\\end{document}")
  }
  private def compile() {
    val p = Runtime.getRuntime().exec(Array("lualatex", file))
    val out = new BufferedReader(new InputStreamReader(p.getInputStream()))
    if (printDebug2) {
      var s: String = out.readLine();
      while (s != null) {
        println(s)
        s = out.readLine()
      }
    }
    p.waitFor()
  }
}