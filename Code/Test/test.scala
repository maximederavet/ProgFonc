/*
* Mock-up test 1
*/

//$ Exercise 1

case class Patient(name: String, height: Double, weight: Double)

val patients = List(
  Patient("a", 1.50, 30.0),
  Patient("b", 1.50, 40.0),
  Patient("c", 1.50, 50.0),
  Patient("d", 1.50, 60.0)
)

// ! We access elements of a list using parentheses: patients(1) == patient b

def BMI(mass: Double, height: Double): Double = mass / (height * height)

def category(patient: Patient): String = {
  val bmi = BMI(patient.weight, patient.height)

  def categoryMatch(bmi: Double): String = {
    if (bmi < 18.5) "underweight"
    else if (bmi < 25 && bmi >= 18.5) "normal"
    else if (bmi >= 25) "overweight"
    else "Not found"
  }

  categoryMatch(bmi)
}

val patientsCategory = patients.map(category)

// OK

// $ Exercise 2:

def processData(patients: List[Patient]): List[(String, Double)] = {

  def loop(i: Int, result: List[(String, Double)]): List[(String, Double)] =
    if (i < patients.length)
      loop(i + 1, result :+ (patients(i).name, BMI(patients(i).weight, patients(i).height)))
    else result

  loop(0, Nil)
}
