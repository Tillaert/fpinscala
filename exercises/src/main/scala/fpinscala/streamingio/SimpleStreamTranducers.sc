object SST {

  sealed trait Process[I, O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }
  }

  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]())
    extends Process[I, O]

  case class Await[I, O](recv: Option[I] => Process[I, O])
    extends Process[I, O]

  case class Halt[I, O]() extends Process[I, O]

  Emit[Int, Int](3)
  Emit[Int, Int](3)(Stream(4, 5, 6)).toList
  Await[Int, Int] {
    case Some(i: Int) => Emit[Int, Int](i)
    case None => Halt()
  }(Stream(1, 2, 3))
  Halt[Int, Int]()(Stream(1, 2, 3))

  def liftOne[I, O](f: I => O): Process[I, O] =
    Await {
      case Some(i) => Emit[I, O](f(i))
      case None => Halt()
    }

  val p = liftOne((x: Int) => x * 2)

  val xs = p(Stream(1, 2, 3)).toList

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  val pr = lift((x:Int) => x * 2)

  val xsr = pr(Stream(1,2,3)).toList

  def filter[I](p: I => Boolean) : Process[I,I] =
    Await[I,I] {
      case Some(i) if p(i) => Emit[I,I](i)
      case _ => Halt()
    }.repeat

  filter[Int](_ % 2 == 0)(Stream(1,2,3,4,5)).toList

  def sum: Process[Double,Double] = {
    def go(acc: Double): Process[Double, Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc))
        case None => Halt()
      }
    go(0.0)
  }

  val s = sum(Stream(1.0, 2,0, 3.0, 4.0)).toList
}


