package com.evolutiongaming.bootcamp.typeclass

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework {
  /**
   * Lo and behold! Brand new super-useful collection library for Scala!
   *
   * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
   * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
   * of the data stored.
   *
   * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
   * a thing called size score. Its calculation rules:
   * - size score of a Byte is 1
   * - Int - 4 (as primitive JVM int consists of 4 bytes)
   * - Long - 8
   * - Char - 2 (one UTF-16 symbol is 2 bytes)
   * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
   * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
   * the fields
   * - score for any sequence (Array[T], List[T], Vector[T]) is
   * 12 (our old friend object header) + sum of scores of all elements
   * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
   */
  object SuperVipCollections4s {
    type SizeScore = Int

    trait GetSizeScore[T] {
      def apply(value: T): SizeScore
    }

    object syntax {
      implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
        def sizeScore: SizeScore = implicitly[GetSizeScore[T]].apply(inner)//implement the syntax!
      }
    }

    /**
     * Mutable key-value cache which limits the size score of the data scored.
     *
     * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
     * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
     * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
     * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
     *
     * @param maxSizeScore max size score for the stored data
     * @tparam K key type
     * @tparam V value type
     */
    final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {
      //with this you can use .sizeScore syntax on keys and values
      import syntax._

      /*
      mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
       */
      private val map = mutable.LinkedHashMap.empty[K, V]

      def put(key: K, value: V): Unit = {
        val mapSize = map.keys.map(_.sizeScore).sum + map.values.map(_.sizeScore).sum

        if ((maxSizeScore < (mapSize + key.sizeScore + value.sizeScore)) && !map.isEmpty){
          map.remove(map.head._1)
          put(key, value)
        }
        else
          map.put(key, value)
      }

      def get(key: K): Option[V] = {
        map.get(key)
      }
    }

    /**
     * Cool custom immutable multi-map collection - does not extend the standard library collection types
     * (yes, this is a feature)
     */
    final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])
    object PackedMultiMap {
      def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()
      def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
    }

    /**
     * Type-class allowing us to iterate over different "collection-like" types with one type arg
     */
    trait Iterate[-F[_]] {
      def iterator[T](f: F[T]): Iterator[T]
    }
    /**
     * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
     */
    trait Iterate2[-F[_, _]] {
      def iterator1[T, S](f: F[T, S]): Iterator[T]
      def iterator2[T, S](f: F[T, S]): Iterator[S]
    }

    object instances {
      import syntax._
      import instances._

      implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
        override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
      }
      //Array is not an Iterable in Scala 2.13 but we still might abstract over iteration logic for both!
      implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
        override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
      }
      //Provide Iterate2 instances for Map and PackedMultiMap!
      //if the code doesn't compile while you think it should - sometimes full rebuild helps!
      implicit val mapIterate2: Iterate2[Map] = new Iterate2[Map] {
        override def iterator1[T, S](f: Map[T, S]): Iterator[T] = f.keys.iterator
        override def iterator2[T, S](f: Map[T, S]): Iterator[S] = f.values.iterator
      }

      implicit val packedMultiMapIterate2: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
        override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.toMap.keys.iterator
        override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.toMap.values.iterator
      }

      /*
      replace this big guy with proper implicit instances for types:
      - Byte, Char, Int, Long
      - String
      - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
        - points to karma if you provide those in a generic way
        (Iterate and Iterate2 type-classes might be helpful!)

      If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
      List and other collections and then replace those with generic instances.
       */
      val sizeJVM: Int = 12

      implicit def byteGetSizeScore: GetSizeScore[Byte] = _ => 1
      implicit def charGetSizeScore: GetSizeScore[Char] = _ => 2
      implicit def intGetSizeScore: GetSizeScore[Int] = _ => 4
      implicit def longGetSizeScore: GetSizeScore[Long] = _ => 8

      implicit def stringGetSizeScore: GetSizeScore[String] = (string: String) => sizeJVM + string.length * 2

      implicit def arrayGetSizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = (array: Array[T]) => sizeJVM + array.map(_.sizeScore).sum
      implicit def listGetSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = (list: List[T]) => sizeJVM + list.map(_.sizeScore).sum
      implicit def vectorGetSizeScore[T: GetSizeScore]: GetSizeScore[Vector[T]] = (vector: Vector[T]) => sizeJVM + vector.map(_.sizeScore).sum
      implicit def mapGetSizeScore[T: GetSizeScore]: GetSizeScore[Map[T, T]] = (map: Map[T, T]) => sizeJVM + map.keys.map(_.sizeScore).sum + map.values.map(_.sizeScore).sum
      implicit def packedMultiMapGetSizeScore[T: GetSizeScore]: GetSizeScore[PackedMultiMap[T, T]] = (packedMultiMap: PackedMultiMap[T, T]) => sizeJVM + packedMultiMap.inner.toMap.keys.map(_.sizeScore).sum + packedMultiMap.inner.toMap.values.map(_.sizeScore).sum

      //implicit def stubGetSizeScore[T]: GetSizeScore[T] = (_: T) => 42
    }
  }

  /*
  Time to bring some business value!
  #GoodVibes #ThrowbackThursday #NoFilter #squadgoals
   */
  object MyTwitter {
    import SuperVipCollections4s._
    import instances._
    import syntax._

    final case class Twit(
                           id: Long,
                           userId: Int,
                           hashTags: Vector[String],
                           attributes: PackedMultiMap[String, String],
                           fbiNotes: List[FbiNote],
                         )

    final case class FbiNote(
                              month: String,
                              favouriteChar: Char,
                              watchedPewDiePieTimes: Long,
                            )

    trait TwitCache {
      def put(twit: Twit): Unit
      def get(id: Long): Option[Twit]
    }

    object Twit{
      implicit val fbiNoteGetSizeScore: GetSizeScore[FbiNote] = (fbiNote: FbiNote) => fbiNote.month.sizeScore + fbiNote.favouriteChar.sizeScore + fbiNote.watchedPewDiePieTimes.sizeScore
      implicit val twitGetSizeScore: GetSizeScore[Twit] = (twit: Twit) => sizeJVM + twit.id.sizeScore + twit.userId.sizeScore + twit.hashTags.sizeScore + twit.attributes.sizeScore + twit.fbiNotes.sizeScore
    }

    /*
    Return an implementation based on MutableBoundedCache[Long, Twit]
     */
    def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {
      val twitMap = new MutableBoundedCache[Long, Twit](maxSizeScore)

      override def put(twit: Twit): Unit = twitMap.put(twit.id, twit)
      override def get(id: Long): Option[Twit] = twitMap.get(id)
    }
  }
}
