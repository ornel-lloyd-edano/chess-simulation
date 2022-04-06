package chess.util

trait ObservableState[T] {
  protected val observers: scala.collection.mutable.ListBuffer[StateObserver[T]]

  def register(observer: StateObserver[T]): Unit = observers.addOne(observer)

  def notifyObservers(state: T): Unit //because notify method name is taken by class Object
}
