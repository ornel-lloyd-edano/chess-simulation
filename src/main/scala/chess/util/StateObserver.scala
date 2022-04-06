package chess.util

trait StateObserver[T] {
  def update(state: T): Unit
}
