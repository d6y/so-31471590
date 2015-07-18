import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

object Example extends App {

  import slick.driver.H2Driver.api._

  final case class EnterpriseRow(id: Int, name: String)

  final class Enterprise(tag: Tag) extends Table[EnterpriseRow](tag, "enterprise") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name  = column[String]("name")
    def * = (id, name) <> (EnterpriseRow.tupled, EnterpriseRow.unapply)
  }

  final case class SalaryRow(int: Int, enterpriseId: Int, worker: String)

  final class Salary(tag: Tag) extends Table[SalaryRow](tag, "salary") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def enterpriseId = column[Int]("enterprise_id")
    def worker  = column[String]("worker")
    def enterprise = foreignKey("salary_ent_fk", enterpriseId, enterprises)(_.id)
    def * = (id, enterpriseId, worker) <> (SalaryRow.tupled, SalaryRow.unapply)
  }

  lazy val enterprises = TableQuery[Enterprise]
  val salaries = TableQuery[Salary]

  val db = Database.forConfig("h2")

  // Initialize the schema:
  val setup = DBIO.seq(
    (enterprises.schema ++ salaries.schema).create,
    enterprises += EnterpriseRow(9, "Big Co")
  )
  Await.result(db.run(setup), 2 seconds)


  case class SalaryItem(enterpriseName: String, worker: String)
  val salaryItem = SalaryItem("Big Inc", "Bob")

  val enterpriseQS = enterprises.
    filter(p => p.name.toUpperCase.trim === salaryItem.enterpriseName.toUpperCase.trim).result

  def createNewEnterprise(salaryItem: SalaryItem): DBIO[Int] = for {
    eId          <- (enterprises returning enterprises.map(_.id)) += EnterpriseRow(0, salaryItem.enterpriseName)
    rowsAffected <- salaries += new SalaryRow(0, eId, salaryItem.worker)
  } yield rowsAffected

  val action: DBIO[Int] = for {
    existingEnterprise <- enterpriseQS.headOption
    rowsAffected       <- existingEnterprise match {
      case Some(n) => salaries += new SalaryRow(0, n.id, salaryItem.worker)
      case None    => createNewEnterprise(salaryItem)
    }
  } yield rowsAffected

  val future: Future[Int] = db.run(action.transactionally)

  val result = Await.result(future, 2 seconds)

  println(s"Result of action is: $result")

  println("\n\nState of the enterprise table:")
  Await.result(db.run(enterprises.result), 2 seconds).foreach(println)

  println("\n\nState of the salaries table:")
  Await.result(db.run(salaries.result), 2 seconds).foreach(println)

  db.close
}