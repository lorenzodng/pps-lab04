package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  //esempio di utilizzo di un trait
  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:
    
    case class Complex(re: Double, im: Double)
    
    def complex(re: Double, im: Double): Complex = Complex(re, im)

    extension (complex: Complex)
      
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = Complex(complex.re + other.re, complex.im + other.im)
      def subtract(other: Complex): Complex = Complex(complex.re - other.re, complex.im - other.im)
      def asString(): String =
        if (complex.re != 0 && complex.im > 0)
          complex.re + " + " + complex.im + "i"
        else if (complex.re != 0 && complex.im < 0)
          complex.re + " - " + (-complex.im) + "i"
        else if (complex.re != 0 && complex.im == 0)
          complex.re + ""
        else if (complex.re == 0 && complex.im != 0)
          complex.im + "i"
        else "0.0"
   





