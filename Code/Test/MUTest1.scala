/*
? Mock-up test 1 
*/

//$ Exercice 1 

case class Patient(name: String, height: Double, weight: Double)

val patients = List(
Patient("a", 1, 30.0),
Patient("b", 1.50, 40.0),
Patient("c", 1.50, 50.0),
Patient("d", 3, 60.0)
)

// ! On accède aux éléments d'une liste avec des parenthèrese : patients(1) == patient b

def BMI(mass: Double, height: Double): Double = mass/(height*height)


def category(patient: Patient): String ={
	val bmi = BMI(patient.weight, patient.height)

	def categoryMatch(bmi: Double): String = {
		if (bmi < 18.5) then "underweight"
		else if (bmi < 25 && bmi >= 18.5) then "normal"
		else if (bmi >= 25) then "overweight"
		else "Not found"
	}

	categoryMatch(bmi)
}

val patientsCategory = patients.map(category)

// OK

// $ Exercice 2 : 

def processData(patients: List[Patient]): List[(String, Double)] = {

	def loop(i: Int, result: List[(String, Double)]): List[(String, Double)] =
		if(i<patients.length)
			loop(i+1, result:+ (patients(i).name, BMI(patients(i).weight, patients(i).height)))
			

		else result


	loop(0, Nil)

}
val data : List[(String, Double)] = processData(patients)
// OK


//$ Exercise X

// Return the list of patient that are overweight : 
val filteredPatients = processData(patients).filter { case (_, bmi) => bmi > 25.0 }


/* Return the sum of weight grouped by category
def sumByCategory(patients: Patient): List[Double] = {

}  */


// Moyenne de toutes les tailles et masses
val averageMass = patients.map(_.height/patients.length).sum
val average = (patients.map(_.height/patients.length).sum , patients.map(_.weight/patients.length).sum)

