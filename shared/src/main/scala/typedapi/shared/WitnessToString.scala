package typedapi.shared

sealed trait WitnessToString[K] {

  def show(key: K): String
}

trait WitnessToStringLowPrio {

  implicit def symbolKey[K <: Symbol] = new WitnessToString[K] {
    def show(key: K): String = key.name
  }

  implicit def stringKey[K <: String] = new WitnessToString[K] {
    def show(key: K): String = key
  }
}
