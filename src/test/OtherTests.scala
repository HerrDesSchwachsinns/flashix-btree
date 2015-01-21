package test

import datatypes.zbranch
import helpers.scala.ArrayWrapperDeep
import datatypes.key.inodekey
import layers.BtreeIter
import layers.BtreeRec
import layers.drawBtree
import java.io.PrintWriter
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.InputStreamReader
import java.io.BufferedReader
import misc.Exec

object OtherTests {
  def main(args: Array[String]) {
    val start = 1
    val end = 20
    val btree = new BtreeRec
    var i = 1
    while (i < start) { Helper.insert(btree, i); i += 1 }
    drawBtree.file = "test.tex"
    Exec(Array("rm", "-f", "drawings/pdf/*"))
    Exec(Array("rm", "-f", "drawings/png/*"))
    while (i <= end) {
      Helper.insert(btree, i)
      if (!btree.draw("test.tex")) {
        println(s"ERROR at $i")
      }
      Exec(Array("cp", "test.pdf", s"drawings/pdf/$i.pdf"))
      Exec(Array("C:/Program Files/ImageMagick-7.0.0-Q16/convert.exe", "-density", "600x600", s"drawings/pdf/$i.pdf", "-quality", "90", "-resize", "1080x800", s"drawings/png/$i.png"))

      i += 1
    }
  }
}