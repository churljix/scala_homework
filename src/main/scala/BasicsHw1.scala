object BasicsHw1 extends App {
  //https://en.wikipedia.org/wiki/Greatest_common_divisor#Euclid's_algorithm
  def gcd(a: Int, b: Int): Int = {
    // when remainder is 0, the previous remainder ir the gcd
    if (b == 0)
      Math.abs(a)
    else
      gcd(b, a % b)
  }

  def lcm(a: Int, b: Int): Int = {
    Math.abs(a * b) / gcd(a, b)
  }
}