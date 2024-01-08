/* Question 1 */
case class Patient(name: String, height: Double, weight: Double)

val patients = List(
	Patient("a", 1.50, 30.0),
	Patient("b", 1.50, 40.0),
	Patient("c", 1.50, 50.0),
	Patient("d", 1.50, 60.0)
)

def BMI(patient : Patient): Double = 
	patient.weight /(patient.height * patient.height)

def category(patient : Patient): String = 
	if(BMI(patient) < 18.5) "Starvin MArvin"
	else if(BMI(patient) > 25) "Cartman"
	else "Normal"

def categories(patients : List[Patient]): List[String] = patients.map(category)

/*ALL good first try ez*/

/* Question 2 */
def processdata(patients : List[Patient]): List[(String, Double)] = 
	if (patients.isEmpty) Nil
	else (patients.head.name , BMI(patients.head)) :: processdata(patients.tail)
	
	// This function is recursive as it does not base itself on a "loop" structure

def processdata2(p : List[Patient]): List[(String, Double)] = 

	val s = p.foldRight(0)((_: Patient, x: Int) => x+1)

	def loop(n: Int): List[(String, Double)] = 
		if (n == 0 ) Nil
		else (p.head.name , BMI(p.head)) :: loop(n-1)
	
	loop(s)

	// This function is iterative is it base itself on a loop strcuture



/* Qeustion X */ 
//Return the list of patient that are overweight 
patients.filter(BMI(_) > 25)

// Return the sum of weight grouped by category
val cat = categories(patients)


// Return the averageheignt of patient using oneliner (here it is one passage in the list)
val x: (Double, Int) = patients.foldRight((0.0, 0))((p: Patient, acc: (Double, Int)) => (acc._1 + p.height, acc._2+1))
val result = x._1 / x._2

// Oneliner
// val averageHeight: Double = patients.foldRight((0.0, 0))((p, acc) => (acc._1 + p.height, acc._2 + 1))._1 / patients.size

//Return the average heigth and wieght
val (averageHeight, averageWeight) = patients.foldRight((0.0, 0.0, 0))((p, acc) => (acc._1 + p.height, acc._2 + p.weight, acc._3 + 1)) match { case (h, w, c) => (h / c, w / c) }

