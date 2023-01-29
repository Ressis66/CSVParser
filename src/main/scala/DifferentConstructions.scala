import DifferentConstructions.Completions.CompletionArg

object DifferentConstructions:

  object Different:
     extension (x: String)
       def + (y: String): Int = {
       val a: String = x + y
     a.toInt
  }

  end Different

  object Completions:

  // The argument "magnet" type
    enum CompletionArg:
      case ShowItIsString(s: String)
      case ShowItIsInt(s: Int)
      case ShowItIsFloat(s: Float)

    object CompletionArg:

     given fromString : Conversion[String, CompletionArg] = ShowItIsString(_)
     given fromInt : Conversion[Int, CompletionArg] = ShowItIsInt(_)
     given fromFloat: Conversion[Float, CompletionArg] = ShowItIsFloat(_)
    end CompletionArg
    import CompletionArg.*

    def complete[T](arg: CompletionArg) = arg match
      case ShowItIsString(_) => arg.toString
      case ShowItIsInt(_) => arg.toString.toInt
      case ShowItIsFloat(_) => arg.toString.toFloat

  end Completions

  object MyMath:

    opaque type Logarithm = Double

    object Logarithm:

    // These are the two ways to lift to the Logarithm type

      def apply(d: Double): Logarithm = math.log(d)

      def safe(d: Double): Option[Logarithm] =
        if d > 0.0 then Some(math.log(d)) else None

    end Logarithm

  // Extension methods define opaque types' public APIs
    extension (x: Logarithm)
      def toDouble: Double = math.exp(x)
      def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
      def * (y: Logarithm): Logarithm = x + y

  end MyMath
end DifferentConstructions