package scalaz_experiments.free_monad

import scalaz._
import Scalaz._

import scala.collection.mutable

sealed trait KVStoreA[A]

case class Put[T](key: String, value: T) extends KVStoreA[Unit]

case class Get[T](key: String) extends KVStoreA[Option[T]]

case class Delete(key: String) extends KVStoreA[Unit]

object KVStoreA {
  type KVStore[A] = Free[KVStoreA, A]

  // Put returns nothing (i.e. Unit).
  def put[T](key: String, value: T): KVStore[Unit] =
    Free.liftF[KVStoreA, Unit](Put[T](key, value))

  // Get returns a T value.
  def get[T](key: String): KVStore[Option[T]] =
    Free.liftF[KVStoreA, Option[T]](Get[T](key))

  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] =
    Free.liftF(Delete(key))

  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}

object TestingKVStore {

  import KVStoreA._

  def program: KVStore[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n


  def impureCompiler: KVStoreA ~> Id.Id =
    new (KVStoreA ~> Id) {

      // a very simple (and imprecise) key-value store
      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: KVStoreA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).map(_.asInstanceOf[A])
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

  type KVStoreState[A] = State[Map[String, Any], A]
  val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
    def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
      fa match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          State(s => {
            (s.updated(key, value), ())
          })
        case Get(key) =>
          println(s"get($key)")
          State(s => {
            (s, s.get(key).map(_.asInstanceOf[A]))
          })
        case Delete(key) =>
          println(s"delete($key)")
          State(s => {
            (s - key, ())
          })
      }
  }

  def main(args: Array[String]): Unit = {
    val result = program.foldMap(impureCompiler)
    println("result: " + result)

    println("***********************")

    val result1 = program.foldMap(pureCompiler).run(Map.empty)
    println(result1)
  }

}


