package fr.upem.partiel

import java.time.Instant.ofEpochMilli
import java.time.LocalDateTime.ofInstant
import java.time.ZoneId.of
import java.time.{LocalDateTime, ZoneOffset}
import java.util.UUID

import fr.upem.partiel.Part2.Category.{Deposit, Withdrawal, parse}
import fr.upem.partiel.Part2.Transaction.Id

import scala.util.{Failure, Success, Try}

// Part2 (10pts)
/**
  *
  * The goal is to create a system that allows users to handle their personal finances.
  * Each user has an account with his UNIQUELY IDENTIFIED transactions.
  * Each transaction can be categorized with categories such as: Salary, Purchase, Withdrawal, Checks (deposits and payments) etc.
  *
  */
object Part2 extends App {

  // 2.1 Modelling.
  // Create a model for the user's bank account
  final case class Account(id: Account.Id = Account.Id(UUID.randomUUID().toString),
                           transactions: List[Transaction] = Nil)

  object Account {

    final case class Id(value: String) extends AnyVal

    implicit def Show(implicit transactionShow: Show[Transaction]) = new Show[Account] {
      override def show(a: Account) = a.transactions.map(transactionShow.show).mkString("\n")
    }

  }

  // Create a model for a transaction (has an amount and a date)
  final case class Transaction(id: Transaction.Id = Id(UUID.randomUUID().toString),
                               amount: Double,
                               date: LocalDateTime = LocalDateTime.now(),
                               category: Option[Category] = None)

  object Transaction {

    final case class Id(value: String = UUID.randomUUID().toString) extends AnyVal

    implicit def TransactionShow(implicit idShow: Show[Transaction.Id],
                                 categoryShow: Show[Category]) = new Show[Transaction] {
      override def show(t: Transaction) =
        s"${idShow.show(t.id)},${t.category.map(categoryShow.show).getOrElse("")},${t.amount},${t.date.toInstant(ZoneOffset.UTC).toEpochMilli}"
    }

    object Id {

      def parse(s: String): Try[Id] = if (s.isEmpty) Failure(new RuntimeException("Invalid Id")) else Success(Id(s))

      implicit val Show = new Show[Id] {
        override def show(i: Id) = s"${i.value}"
      }

    }

  }

  // Create a model for the following categories [salary, purchase, check deposit, check payment, withdrawal]
  sealed trait Category {
    val name: String
  }

  object Category {

    final case object Salary extends Category {
      override val name = "salary"
    }

    final case object Purchase extends Category {
      override val name = "purchase"
    }

    final case object Deposit extends Category {
      override val name = "check-deposit"
    }

    final case object Payment extends Category {
      override val name = "check-payment"
    }

    final case object Withdrawal extends Category {
      override val name = "withdrawal"
    }

    def parse(s: String): Try[Category] = s match {
      case Salary.name => Success(Salary)
      case Purchase.name => Success(Purchase)
      case Deposit.name => Success(Deposit)
      case Payment.name => Success(Payment)
      case Withdrawal.name => Success(Withdrawal)
      case _ => Failure(new RuntimeException("Invalid category"))
    }

    val Incomes = List(Salary, Deposit, Payment)
    val Checks = List(Deposit, Payment)

    implicit val Show = new Show[Category] {
      override def show(c: Category) = c.name
    }

  }

  // 2.2 Create api
  // Create an api that allows for:
  // - Adding a transaction to a bank account
  // - Adding a transaction to a bank account with it's category
  // - Categorizing or recategorizing an existing transaction
  //
  // help: The bank account must save, for each transaction id, the transaction and it's eventual category
  // This could be achieved through a structure of this kind:
  // BankAccount(transactions: Map[TransactionId, CategorizedTransaction])

  object Api {
    def addTransaction(account: Account)(id: Transaction.Id, amount: Double): Account = {
      val transaction = Transaction(id = id, amount = amount)
      account.copy(transactions = transaction :: account.transactions)
    }

    def addCategorizedTransaction(account: Account)(id: Transaction.Id, amount: Double, category: Category): Account = {
      val transaction = Transaction(id = id, amount = amount, category = Some(category))
      account.copy(transactions = transaction :: account.transactions)
    }

    def categorize(account: Account)(id: Transaction.Id, category: Category): Account =
      account.copy(transactions = account.transactions.map {
        case Transaction(`id`, a, d, _) => Transaction(id, a, d, Some(category))
        case transaction => transaction
      })
  }

  // 2.3 Use the api that you just created.
  // - Create an empty account
  val account = Account()

  // - Add a transaction with id 1 of amount -13 (and any date)
  val one = Api.addTransaction(account)(Id("1"), -13)
  // - Add a transaction with id 2 of amount -50 (and any date)
  val two = Api.addTransaction(one)(Id("2"), -50)
  // - Add a check payment with id 3 of amount 650 (and any date)
  val three = Api.addCategorizedTransaction(two)(Id("3"), 650, Category.Payment)
  // - Categorize the second transaction (id "2") as a withdrawal
  val four = Api.categorize(three)(Id("2"), Withdrawal)
  // - (Re)categorize the third transaction (id "3") as check deposit
  val five = Api.categorize(four)(Id("3"), Deposit)
  //
  // help: After the above operations the bank account should hold:
  // TransactionId(1) -> (Transaction(1, -13, date), None)
  // TransactionId(2) -> (Transaction(2, -50, date), Some(Withdrawal))
  // TransactionId(3) -> (Transaction(3, 650, date), Some(CheckDeposit)

  println(five.transactions) // List(
  //  Transaction(Id(3),650.0,2019-01-16T00:12:53.948,Some(Deposit)),
  //  Transaction(Id(2),-50.0,2019-01-16T00:12:53.947,Some(Withdrawal)),
  //  Transaction(Id(1),-13.0,2019-01-16T00:12:53.944,None)
  // )


  // 2.4 CSV Export
  // Users want to be able to export their accounts in CSV (Comma-Separated Values) format.
  // A line is structured as follows: Id, Type, Amount, Date
  // Allow exporting a bank account as a CSV (no need to write a file, just write a String).
  // Amounts do not need to be formatted, write dates in any valid format (timestamp, ISO-8601 ...)
  //
  // Example output:
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770

  trait Show[A] {
    def show(a: A): String
  }

  def export[A: Show](a: A) = implicitly[Show[A]].show(a)

  // 3,check-deposit,650.0,1547598768128
  // 2,withdrawal,-50.0,1547598768127
  // 1,,-13.0,1547598768123
  println(export(five))

  // 2.5 CSV Import
  // Users want to be able to import transactions from a CSV.
  // Write code to parse and validate csv files
  // Validation: The input data should be validated
  //
  // Example valid input
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770
  //
  // Example invalid input
  // 1,invalid type,invalid amount,invalid date

  def accountImport(s: String): Try[List[Transaction]] = {

    def swap[A](l: List[Try[A]]): Try[List[A]] =
      l match {
        case Success(v) :: xs => swap(xs).map(x => v :: x)
        case Failure(e) :: _ => Failure(e)
        case Nil => Success(Nil)
      }

    def parseTransaction(s: String) = {
      s.split(",") match {
        case Array(i, c, a, t) => for {
          id <- Transaction.Id.parse(i)
          category <- parse(c).map(Option.apply).recover { case _ => None }
          amount <- Try(a.toDouble)
          date <- Try(ofInstant(ofEpochMilli(t.toLong), of("UTC")))
        } yield Transaction(id, amount, date, category)
        case _ =>
          Failure(new RuntimeException("Invalid transaction"))
      }
    }

    swap(s.split("\n").toList.map(parseTransaction))
  }

  println(accountImport(",invalid type,invalid amount,invalid date")) // Failure(java.lang.RuntimeException: Invalid Id)
  println(accountImport("1,invalid type,invalid amount,invalid date")) // Failure(java.lang.NumberFormatException: For input string: "invalid amount")
  println(accountImport("1,invalid type,60,invalid date")) // Failure(java.lang.NumberFormatException: For input string: "invalid date")
  println(accountImport("1,invalid type,60,1546784990415")) // Success(List(Transaction(Id(1),60.0,2019-01-06T14:29:50.415,None)))

  // 2.6 Extend the api data analysis features
  // It should allow for:
  // - Sum all incomes (salaries, check deposits, uncategorized positive transactions)
  // - List all check (deposit and payment) operations
  // - Compute the account balance
  object Analysis {

    def sumIncomes(account: Account): Double =
      account
        .transactions
        .filter(t => t.category.exists(Category.Incomes.contains) || (t.category.isEmpty && t.amount > 0))
        .map(_.amount)
        .sum

    def listChecks(account: Account): List[Transaction] =
      account
        .transactions
        .filter(t => t.category.exists(Category.Checks.contains))

    def balance(account: Account): Double = account.transactions.map(_.amount).sum
  }

  println(Analysis.sumIncomes(four)) // 650.0
  println(Analysis.listChecks(four)) // List(Transaction(Id(3),650.0,2019-01-16T00:45:38.899,Some(Payment)))
  println(Analysis.balance(four)) // 587.0

}
