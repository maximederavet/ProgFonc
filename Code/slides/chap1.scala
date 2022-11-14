object FactorialModule{
    def fac(n: Int): Int = {
        if(n==0) 1
        else n *fac(n-1)
    }


    private def formatFac(n: Int) = { //Private n'est pas important pour ce cours-ci 
        val messsage = "The ffactorial of %d is %d"
        message.format(n, fac(n))
    }

    def main(args: Array[String]): Unit =
        println(formatFac(5))
}

/*
Les {} ne sont pas obligatoire en Scala 3 pour englober les focntions et les modules


Pour run le code, plusieurs options:


Le compiler et l'exécuter:
$ scalac FactorialModule.scala
$ scala FactorialModule


Pour les petits fichier on peut passer la compilation
$ scala FactorialModule.scala


Ou lancer scala dans le terminal
$ scala
Welcome to Scala 3.1.2 (17.0.2, Java Java HotSpot(TM) 64-Bit Server VM).
Type in expressions for evaluation. Or try :help.

scala> :load FactorialModule.scala
// defined object FactorialModule

scala> FactorialModule.fac(7)
val res0: Int = 5040

scala> :quit




*/