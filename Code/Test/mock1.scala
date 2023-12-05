case class Patient(name: String, height: Double, weight: Double)


val patients = List(
Patient("a", 1.50, 30.0),
Patient("b", 1.50, 40.0),
Patient("c", 1.50, 50.0),
Patient("d", 1.50, 60.0)
)

def bmi(patient: Patient): Double = 
	patient.weight / (patient.height*patient.height) //! aux priorité des opérations

def category(patient: Patient): String = 
	val bmip = bmi(patient)									 //! ne peut pas être le même nom que la fonction
	if (bmip < 18.5) "Underweight"
	else if (bmip >= 25) "Overweight"
	else "Normal"

val categories = patients.map(category) 				 //! Attention à l'utilisation de .map



// exercice 2 
def processdata(patients: List[Patient]): List[(String, Double)] = {
	if (patients.isEmpty) Nil
	else (patients.head.name, bmi(patients.head)) +: processdata(patients.tail) //! Attention +: sans List() pour créer une liste récursive
}

val rs0 = processdata(patients)

def processdata2(patients: List[Patient]): List[(String, Double)] = {
	def loop(n: Int): List[(String, Double)] = {
		if (n >= patients.size) Nil
		else (patients(n).name, bmi(patients(n))) +: loop(n+1)
	}
	loop(0)
}

def processdata3(patients: List[Patient]): List[(String, Double)] = {
  	patients.foldRight(List[(String, Double)]()) {
	(patient, acc) => (patient.name, bmi(patient)) :: acc
	}
} 



//Exercice X 

//Return the list of overweight patients 
val rs1 = processdata(patients).filter{case (_, bmi) => bmi >= 25}


//Average height of patients 
val rs2 = (patients.map(_.height).sum / patients.size)

//average height and weight 
val rs3 = List((patients.map(_.height).sum , (patients.map(_.weight).sum))).map( (x, y) => (x/patients.size , y/patients.size) )

// Sum of weight grouped by bmi category
val rs4 = patients.groupBy(category).map{case (cat, pats) => (cat, pats.map(_.weight).sum)}  //! ? 