package misc

import java.io.BufferedReader
import java.io.InputStreamReader

object Exec {
  def apply(a: Array[String]) {
    val p = Runtime.getRuntime().exec(a)
    val out = new BufferedReader(new InputStreamReader(p.getInputStream()))
    val err = new BufferedReader(new InputStreamReader(p.getErrorStream))
    var s: String = out.readLine();
    while (s != null) {
      System.out.println(s)
      s = out.readLine()
    }
    s = err.readLine()
    while (s != null) {
      System.err.println(s)
      s = err.readLine()
    }
    p.waitFor()
  }
}