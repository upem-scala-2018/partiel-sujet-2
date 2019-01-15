package fr.upem.partiel

import java.time.Instant
import java.time.temporal.ChronoUnit.YEARS

import scala.annotation.tailrec
import scala.util.Try


// Part 1 (10pts)
object Part1 {

  // 1.1 Apply 'mul2' using pattern matching to the given integer (.5pts)
  def mul2(i: Int): Int = i * 2

  def applyMul2WithPatternMatching(i: Option[Int]): Option[Int] = i match {
    case Some(x) => Some(mul2(x))
    case None => None
  }

  // 1.2 Apply 'mul2' WITHOUT using pattern matching to the given integer (.5pts)
  def applyMul2WithoutPatternMatching(i: Option[Int]): Option[Int] = i.map(mul2)

  // 1.3 Refactor the following code using pattern matching (1pts)
  sealed trait Animal

  case object Cat extends Animal

  case object Bird extends Animal

  case class Dog(age: Int) extends Animal

  // Pas besoin du default case car le pattern matching est exhaustif (sealed trait)
  def formatAnimal(animal: Animal): String = animal match {
    case Cat =>
      "It's a cat"
    case Bird =>
      "It's a bird"
    case Dog(age) =>
      s"It's a $age year old dog"
  }

  // 1.4 Find the index of the given element if any, use recursivity (1pts)

  // Cette méthode n'est pas récursive terminale
  def indexOf[A](l: List[A], a: A): Option[Int] = l match {
    case `a` :: _ => Some(0)
    case _ :: xs => indexOf(xs, a).map(_ + 1)
    case Nil => Option.empty
  }

  // 1.4 bis
  def indexOf2[A](l: List[A], a: A): Option[Int] = {

    // Je laisse cette méthode non accessible depuis l'extérieur car c'est un détail d'implémentation
    @tailrec
    def _indexOf(l: List[A], a: A, i: Int): Option[Int] = {
      l match {
        case Nil => Option.empty
        case x :: _ if x == a => Some(i)
        case _ :: xs => _indexOf(xs, a, i + 1)
      }
    }

    _indexOf(l, a, 0)

  }

  // 1.5 Throw away all errors (.5pts)
  case class Error(message: String)

  // collect se comporte comme un filter + map
  def keepValid[A](l: List[Either[Error, A]]): List[A] = l.collect {
    case Right(x) => x
  }

  // 1.6 Aggregate values (.5pts)
  def aggregate[A](l: List[A], combine: (A, A) => A, empty: A): A = l.foldLeft(empty)(combine)

  // 1.7 Aggregate valid values (.5pts)

  // C'est la même chose que précédemment sauf qu'on applique la fonction keepValid sur la liste
  def aggregateValid[A](l: List[Either[Error, A]], combine: (A, A) => A, empty: A): A = aggregate(keepValid(l), combine, empty)

  // 1.8 Create the Monoid typeclass and rewrite the above "aggregateValid" (.5pts)
  trait Monoid[A] {
    def empty: A

    def combine: (A, A) => A
  }

  // Plus besoin de combine et empty dans la signature, car on les trouve dans Monoid
  def aggregateValidM[A](l: List[Either[Error, A]])(implicit ev: Monoid[A]): A = aggregate(keepValid(l), ev.combine, ev.empty)

  // 1.9 Implement the Monoid typeclass for Strings and give an example usage with aggregateValidM (.5pts)
  implicit val StringMonoid = new Monoid[String] {
    override def empty = ""

    override def combine = (x, y) => x ++ y
  }

  aggregateValidM[String](List(Right("4"), Left(Error("Error")), Right("2")))

  // 1.10 Refactor the following object oriented hierarchy with an ADT (1.5pts)
  sealed trait FinancialAsset {
    def computeEarnings: Double
  }

  sealed trait FlatRateAsset extends FinancialAsset {
    protected val rate: Double
    protected val amount: Double

    override def computeEarnings: Double = amount + (amount * rate)
  }

  object LivretA {
    val Rate: Double = 0.75
  }

  final case class LivretA(override val amount: Double) extends FlatRateAsset {
    override protected val rate: Double = LivretA.Rate
  }

  object Pel {
    val Rate: Double = 1.5
    val GovernmentGrant: Int = 1525
  }

  final case class Pel(override val amount: Double, creation: Instant) extends FlatRateAsset {
    override protected val rate: Double = Pel.Rate

    override def computeEarnings: Double =
      if (Instant.now().minus(4, YEARS).isAfter(creation))
        super.computeEarnings + Pel.GovernmentGrant
      else
        super.computeEarnings
  }

  object CarSale {
    val StateHorsePowerTaxation: Int = 500
  }

  final case class CarSale(amount: Int, horsePower: Int) extends FinancialAsset {
    override def computeEarnings: Double = amount - (CarSale.StateHorsePowerTaxation * horsePower)
  }

  // 1.11 Extract the "computeEarnings" logic of the above hierarchy
  // into an "Earnings" typeclass and create the adequate instances (1.5pts)
  trait Earning[A] {
    def computeEarning(a: A): Double
  }

  object FlatRateAsset {
    def computeEarnings(amount: Double, rate: Double): Double = amount + (amount * rate)
  }

  implicit val FinancialAssetEarning = new Earning[FinancialAsset] {
    override def computeEarning(a: FinancialAsset) = a match {
      case LivretA(amount) => FlatRateAsset.computeEarnings(amount, LivretA.Rate)
      case p@Pel(_, _) => PelEarning.computeEarning(p)
      case CarSale(amount, horsePower) => amount - (CarSale.StateHorsePowerTaxation * horsePower)
    }
  }

  val PelEarning: Earning[Pel] = (p: Pel) => if (Instant.now().minus(4, YEARS).isAfter(p.creation))
    FlatRateAsset.computeEarnings(p.amount, Pel.Rate) + Pel.GovernmentGrant
  else
    FlatRateAsset.computeEarnings(p.amount, Pel.Rate)


  // 1.12 Rewrite the following function with your typeclass (.5pts)
  def computeTotalEarnings[A: Earning](assets: List[A]): Double =
    assets.map(implicitly[Earning[A]].computeEarning).sum

  // 1.13 Enrich the "String" type with an "atoi" extension method that parses the
  // given String to an Int IF possible (1pts)
  implicit class StringPlus(val string: String) extends AnyVal {
    def atoi: Try[Int] = Try(string.toInt)
  }

}
