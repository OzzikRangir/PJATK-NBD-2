import scala.collection.immutable.SortedMap

object PatternMatch {
  def matchDayOfWeek(x: String): String =
    x match {
      case "poniedziałek" => "Praca"
      case "wtorek"       => "Praca"
      case "środa"        => "Praca"
      case "czwartek"     => "Praca"
      case "piątek"       => "Praca"
      case "sobota"       => "Weekend"
      case "niedziela"    => "Weekend"
      case _              => "Nie ma takiego dnia"
    }
}

class KontoBankowe(val _stanKonta: Integer) {
  private var stanKonta = _stanKonta;
  def stan = stanKonta

  def wplata(x: Integer) {
    stanKonta += x;
  }

  def wyplata(x: Integer) {
    stanKonta -= x;
  }

  def this() {
    this(0)
  }

}
object Osoby {
  var lista = Array(
    new Osoba("Andrzej", "Barabasz"),
    new Osoba("Tytus", "Bomba"),
    new Osoba("Michał", "Głuś")
  )
  case class Osoba(val firstName: String, val lastName: String) {}

  def matchPerson(pers: Osoba): String =
    pers match {
      case Osoba(_, "Bomba")  => "Rozkaz panie Kapitanie"
      case Osoba("Michał", _) => "Michele, gdzie pizza?"
      case _                  => "Pomyłka, nie chcemy pizzy"
    }
}

object Funct {

  def increment(x: Int): Int = x + 1

  def function(x: Int)(f: (Int) => Int): Int = {
    var y = x
    for (w <- 1 to 3) y = f(y);
    return y
  }
}
object Podatki {
  abstract class Osoba {
    private var _imie: String = "John"
    private val _nazwisko: String = "Doe"
    private var _podatek: Double = 1

    def imie = _imie
    def nazwisko = _nazwisko
    def podatek = _podatek
  }

  class Student extends Osoba {
    override def podatek: Double = 0
  }

  trait Pracownik extends Osoba {
    private var _pensja: Double = 0

    def pensja = _pensja
    def pensja_=(nowaPensja: Double) = _pensja = nowaPensja * podatek

    override def podatek: Double = 0.8
  }

  trait Nauczyciel extends Pracownik {
    override def podatek: Double = 0.9
  }
}

object Main extends App {
  var test = "";
  var student = new Podatki.Student {}
  var pracownik = new Podatki.Pracownik {}
  pracownik.pensja = 2137
  var nauczyciel = new Podatki.Nauczyciel {}
  nauczyciel.pensja = 3000

  var assignments = SortedMap(
    1 -> SortedMap(
      "a" -> PatternMatch.matchDayOfWeek("poniedziałek"),
      "b" -> PatternMatch.matchDayOfWeek("niedziela"),
      "c" -> PatternMatch.matchDayOfWeek("fajniedziałek")
    ),
    2 -> SortedMap(
      "a" -> new KontoBankowe()._stanKonta,
      "b" -> new KontoBankowe(2137)._stanKonta
    ),
    3 -> SortedMap(
      "a" -> Osoby.matchPerson(Osoby.lista(0)),
      "b" -> Osoby.matchPerson(Osoby.lista(1)),
      "c" -> Osoby.matchPerson(Osoby.lista(2))
    ),
    4 -> String.valueOf(Funct.function(2137)(Funct.increment)),
    5 -> SortedMap(
      "a" -> ("student -> podatek: " + student.podatek),
      "b" -> ("nauczyciel -> podatek: " + nauczyciel.podatek + " pensja: " + nauczyciel.pensja),
      "c" -> ("pracownik -> podatek: " + pracownik.podatek + " pensja: " + pracownik.pensja)
    )
  )

  for (assignment <- assignments) {
    if (assignment._2.getClass() != test.getClass())
      for (sub <- assignment._2.asInstanceOf[Map[String, Map[String, String]]])
        println(assignment._1 + ". " + sub._1 + ": " + sub._2)
    else
      println(assignment._1 + ": " + assignment._2)
  }
}
