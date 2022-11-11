
//Interet composé 

def composedInterest(investment: Int, time: Int): Float ={

    val investment2 = investment + investment/10 + 2400  //Un rendement de 10% plus un apport de 200€ par mois

    if (time <=1) investment 
    else composedInterest(investment2, time-1)

} 

val result = (((composedInterest(1,46) - composedInterest(1,45))/100)*70)/12 //Ce qu'on gagne par mois net à 65ans


