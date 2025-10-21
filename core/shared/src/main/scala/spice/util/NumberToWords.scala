package spice.util

object NumberToWords {
  private val units = Vector(
    "Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
    "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"
  )
  private val tens = Vector("", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety")

  // (scaleValue, scaleName), high → low
  private val scales: Vector[(Long, String)] = Vector(
    1_000_000_000_000_000_000L -> "Quintillion",
    1_000_000_000_000_000L -> "Quadrillion",
    1_000_000_000_000L -> "Trillion",
    1_000_000_000L -> "Billion",
    1_000_000L -> "Million",
    1_000L -> "Thousand"
  )

  def apply(n: Long): String = toWords(n)

  def toWords(n: Long): String = {
    if (n == 0L) "Zero"
    else if (n < 0L) s"Negative ${toWords(-n)}"
    else {
      val (rem, parts) = scales.foldLeft((n, Vector.empty[String])) {
        case ((value, acc), (scale, name)) =>
          val count = value / scale
          if (count > 0) {
            val nextAcc = acc :+ spellBelowThousand(count.toInt) :+ name
            (value % scale, nextAcc)
          } else (value, acc)
      }
      val all = if (rem > 0) parts :+ spellBelowThousand(rem.toInt) else parts
      all.mkString(" ")
    }
  }

  private def spellBelowThousand(n: Int): String = {
    val hundreds = n / 100
    val rest = n % 100
    val head =
      if (hundreds > 0) Vector(units(hundreds), "Hundred") else Vector.empty[String]
    val tail =
      if (rest == 0) Vector.empty[String]
      else if (rest < 20) Vector(units(rest))
      else {
        val t = tens(rest / 10)
        val u = rest % 10
        if (u == 0) Vector(t) else Vector(t, units(u))
      }
    (head ++ tail).mkString(" ")
  }
}